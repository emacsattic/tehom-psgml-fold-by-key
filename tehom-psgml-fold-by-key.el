;;; tehom-psgml-fold-by-key.el --- Fold/unfold XML according to keywords

;; Copyright (C) 2001 by Tom Breton

;; Author: Tom Breton <tob@world.std.com>
;; Keywords: hypermedia

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; What it does:

;; When writing an XML or SGML document, using a dtd that supports
;; keywords, you can add keywords describing what its part talk about.
;; (That's always been part of psgml)

;; Then you can interactively hide or show sections of the document
;; according to keywords.  They don't have to be contiguous or in any
;; special relation to each other.  This package finds them according
;; to keywords.

;; This allows you to focus on the sections that interest you at the
;; moment, without worrying about where you put them.

;; Entry points: `tehom-psgml-fbk-refold' is the main entry point.  If
;; you've edited the document and changed its keywords, use
;; `tehom-psgml-fbk-refresh-keys'.  That's all.

;; About the code:

;; This is quick and dirty.  Opportunities for optimization abound: it
;; collects and compares on strings, whereas it could use symbols.  It
;; doesn't know anything about the dtd, so it explores subtrees that
;; can't hold keys.  It unfolds and refolds and re^n-folds.  But it
;; works and it saves me time.

;; The set operations require `cl' at runtime.  To recode them all
;; would be a real chore, and would mean including a fair chunk of
;; `cl' in this package anyways, so I just use `cl'.

;;; Code:

;;;Pre-requisites:

(require 'cl)
(require 'psgml)
(require 'tehom-psgml)
(require 'arrange)

;;; Variables

(defvar tehom-psgml-fbk-keylist '() 
   "The keywords in the current document.
A buffer-local variable." )
(make-variable-buffer-local 'tehom-psgml-fbk-keylist)

(defvar tehom-psgml-fbk-visible-keylist '() 
   "The keywords currently visible in the current document.
A buffer-local variable." )
(make-variable-buffer-local 'tehom-psgml-fbk-visible-keylist)


;;; Keyword acquisition functions

(defun tehom-psgml-direct-keys-of (el)
   "Find the keys directly in EL."
   
   (let*
      ((key-attval (sgml-element-attval el "keys")))
      ;;Parse the string, if any, otherwise return nil.
      (if key-attval
	 (split-string key-attval))))

(defun tehom-psgml-find-keys-recurse (el)
   "Return a list of strings, which are all the keys in the subtree.
EL's own keys are included by this."
   (let
      ((known-keys (tehom-psgml-direct-keys-of el)))

      (tehom-psgml-iterate-children
	 (c el t known-keys)
	 (let
	    ((childrens-keys (tehom-psgml-find-keys-recurse c)))

	    ;;Collect the new keys.  Union them.
	    (callf union known-keys childrens-keys :test #'string=)))))

;;; Folding functions

(defun tehom-psgml-fbk-refold-aux (new-visible-keys)
   "Fold/unfold everything according to NEW-VISIBLE-KEYS."
   ;;Unfold everything
   (sgml-unfold-all)
   ;;Fold just the folded sections.
   (tehom-psgml-fold-some-children-by-keys 
      (sgml-top-element)
      new-visible-keys))


(defun tehom-psgml-fold-some-children-by-keys (el keys)
   "Fold any parts of EL's subtree that don't match KEYS"
   
   (let*
      ((own-keys (tehom-psgml-direct-keys-of el))
	 (match 
	    (or
	       ;;El matches if it has some of the same keys...
	       (intersection own-keys keys  :test #'string=)
	       ;;...or any of its children do.
	       (let ((some-child-matches nil))
		  (tehom-psgml-iterate-children
		     (c el t some-child-matches)
		     (let
			((this-child-matches
			    (tehom-psgml-fold-some-children-by-keys
			       c keys)))

			(when this-child-matches
			   (callf or some-child-matches t))))))))
      
      ;;If there were no matching keys, fold it.
      (unless match
	 (tehom-psgml-fold-given-element el))
      
      match))


(defun tehom-psgml-fold-given-element (el)
   "Fold the element EL."
   
   (sgml-fold-region 
      (sgml-element-start el)
      (sgml-element-end el)))


;;; Initialization

(defun tehom-psgml-fbk-ensure-init ()
   "Ensure that tehom-psgml-fbk-keylist is initted for the current buffer."
   (when (null tehom-psgml-fbk-keylist)
      (tehom-psgml-fbk-refresh-keys)))

;;; User interface functions, Entry points

(defun tehom-psgml-fbk-refresh-keys ()
   "Refresh the set of keywords in the document."
   (interactive)
   (setq tehom-psgml-fbk-keylist
      (tehom-psgml-find-keys-recurse (sgml-top-element))))

(defun tehom-psgml-fbk-refold ()
   "Interactively refold an XML or SGML document by keywords."
   
   (interactive)
   (tehom-psgml-fbk-ensure-init)
   (let*
      (
	 (invisible-keys
	    (set-difference
	       tehom-psgml-fbk-keylist
	       tehom-psgml-fbk-visible-keylist
	       :test #'string=))
	 
	 ;;Arrange starts from the current arrangement.
	 (new-visible-keys
	  (arrange-strings-other-window
	     tehom-psgml-fbk-visible-keylist
	     "Currently visible keywords.  "
	     invisible-keys)))

      (tehom-psgml-fbk-refold-aux new-visible-keys)
      (setq tehom-psgml-fbk-visible-keylist new-visible-keys)))

;;; Epilog

(provide 'tehom-psgml-fold-by-key)

;;; tehom-psgml-fold-by-key.el ends here