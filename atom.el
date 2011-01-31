;;; atom.el --- Create an Atom feed

;; Copyright (C) 2011  Frédéric Perrin

;; Author: Frédéric Perrin <frederic.perrin@resel.fr>
;; Keywords: www, hypermedia, atom, rss

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a library for creating an Atom feed from a Lisp program.
;; The normal usage is to create a feed with `atom-create', giving it
;; a title and a Web address. Once the feed has been created, entries
;; may be added to the feed, by specifying (at the minimum) a title, a
;; permanent link and the content of the entry. Text-only, HTML and
;; XHTML entries are supported.

;; A feed is really a Lisp structure as used by the `xml.el' package,
;; without the parent `feed' element.

;; A typical usage would look like this:

;; (let ((my-atom-feed (atom-create "My feed" "http://example.org")))
;;   ; A simple, text-only entry
;;   (atom-add-text-entry
;;    my-atom-feed
;;    "Hello world"
;;    "http://example.org/hello"
;;    "Hello the world!")
;;
;;   ; A text-only entry, with all the optional pieces of data
;;   (atom-add-text-entry
;;    my-atom-feed
;;    "Bonjour"
;;    "http://example.org/bonjour"
;;    "Bonjour à tout le monde !"
;;    ;; optional: the last modification time
;;    (date-to-time "2011-01-30 23:40:12")
;;    ;; optional: an identifier for this entry; a common way to generate it is
;;    ;; to use the domain name and the creation date of the entry.
;;    (atom-generate-id "http://example.org"
;; 		     (date-to-time "2011-01-30 10:01:05"))
;;    ;; optional: a summary for this entry
;;    "Bonjour, monde.")
;;
;;   (atom-add-xhtml-entry
;;    my-atom-feed
;;    "An XHTML example"
;;    "http://example.org/html-example"
;;    "<p>One can also use <acronym>XHTML</acronym> in the entries.</p>")
;;   (atom-print my-atom-feed))

;;; Code:

(require 'xml)

(defun atom-create (title link &optional author self updated id)
  "Create a new atom structure.

TITLE is the title for the feed, a short, text-only, human
readable string.

AUTHOR is the author of the feed. See `atom-massage-author' for
the possible ways to specify it.

SELF is the canonical URL to this feed.

LINK is the URL of a page responible for the content of this
feed.

UPDATED is the date the feed was last updated. If not given,
`(current-time)' is used.

ID is a unique identifier for this feed. If not given, it
defaults to LINK."
  (let ((atom-feed (list (list 'title nil title))))
    (atom-modify-entry atom-feed 'link (list (list (cons 'href link))))
    (atom-modify-entry atom-feed 'author (atom-massage-author author))
    (if self (atom-modify-entry atom-feed 'link
				`(((href . ,self) (rel . "self")
				   (type . "application/atom+xml")))))
    (atom-modify-entry atom-feed 'updated (atom-format-time updated))
    (atom-modify-entry atom-feed 'id (or id link))
    atom-feed))

(defun atom-push-entry (atom entry)
  "Add the entry ENTRY to the feed ATOM."
  (nconc atom (list `(entry nil . ,entry))))

(defun atom-modify-entry (entry name val)
  "Set the NAME element of ENTRY to VAL. A true MULTIPLE means
to add a new element instead of updating it when it already exists."
  (let ((elem (if (stringp val)
		  (list name nil val)
		(cons name val))))
    (nconc entry (list elem))))

(defun atom-add-entry (atom title link content
			    &optional updated id summary)
  "Add an entry to the atom flux ATOM. Return the newly added
entry.

TITLE is a short, text-only, human readable string.

LINK is a permanent link for this entry. For a given entry, LINK
may change between successive generations of the atom feed.

CONTENT is the content of the entry; use `atom-add-html-entry'
or `atom-add-xhtml-entry' when CONTENT is not text-only.

If SUMMARY is not given, the entry will not contain any summary.

UPDATED defaults to `(current-time)' if omitted, which is
probably not a very good default.

ID defaults to LINK, which is not optimal; see `atom-generate-id'
for a way to create good identifiers. For a given entry, it must
not change between successive generations of the atom feed, even
when the content of the entry ."
  (let ((entry (list (list 'title nil title))))
    (atom-modify-entry entry 'link  (list (list (cons 'href link))))
    (atom-modify-entry entry 'id (or id link))
    (atom-modify-entry entry 'updated (atom-format-time updated))
    (if summary (atom-modify-entry entry 'summary summary))
    (atom-modify-entry entry 'content content)
    (atom-push-entry atom entry)
    entry))

(defalias 'atom-add-text-entry 'atom-add-entry
  "Add an entry to ATOM, with a textual content. See
`atom-add-entry' for details.")

(defun atom-add-html-entry (atom title link content
				  &optional updated id summary)
  "Add an entry to ATOM, with some HTML content. CONTENT should
be a string enconding a valid HTML fragment. See `atom-add-entry'
for additional details."
  (atom-add-entry atom
   title link
   (atom-massage-html content)
   (and summary (atom-massage-html summary))
   updated id))

(defun atom-add-xhtml-entry (atom title link content
				  &optional updated id summary)
  "Add an entry to ATOM, with some XHTML content. CONTENT may be
given either as a string, or as an XML tree, of a valid XHTML
fragment. See `atom-add-entry' for additional details."
  (atom-add-entry atom
   title link
   (atom-massage-xhtml content)
   (and summary	(atom-massage-xhtml summary))
   updated id))

(defun atom-print (atom)
  "Print the Atom feed ATOM in the current buffer."
  (insert "<?xml version=\"1.0\" encoding=\"utf-8\"?>\n")
  (insert "<feed xmlns=\"http://www.w3.org/2005/Atom\">\n")
  (xml-print atom)
  (insert "\n</feed>"))


(defun atom-format-time (&optional time)
  "Format a time according to RFC3339."
  ;; The time zone must be specified in numeric form, but with a colon between
  ;; the hour and minute parts.
  (replace-regexp-in-string
   "\\(..\\)$"
   ":\\1"
   (format-time-string "%Y-%m-%dT%T%z" time)))

(defun atom-massage-html (content)
  "Massage CONTENT so it can be used as an HTML fragment in an
Atom feed. CONTENT must be a string."
  (list '((type . "html")) content))

(defun atom-string-to-xml (string)
  "Convert STRING into a Lisp structure as used by `xml.el'."
  (with-temp-buffer
    (insert string)
    (xml-parse-region (point-min) (point-max))))

(defun atom-massage-xhtml (content)
  "Massage CONTENT so it can be used as an XHTML fragment in an
Atom feed."
  (list '((type . "xhtml"))
	`(div ((xmlns . "http://www.w3.org/1999/xhtml"))
	      . ,(or (and (stringp content)
			  (atom-string-to-xml content))
		     content))))

(defun atom-massage-author (author)
  "Return an XML node representing the author. AUTHOR can be:
- nil, in which case `user-full-name' and `user-mail-address' are
  used;
- a single string, the full name of the author;
- a list with two elements, the full name and the email address
  of the author;
- something else, assumed to be a complete `atomPersonConstruct'."
  `(nil .
	,(cond
	  ((null author) `((name nil ,user-full-name)
			   (email nil ,user-mail-address)))
	  ((stringp author) `((name nil ,author)))
	  ((= 2 (length author)) `((name nil ,(car author))
				   (email nil ,(cadr author))))
	  (t `(author nil ,author)))))

(require 'url-parse)

(defun atom-generate-id (link creation-date)
  "Generate a string suitable for use as an atom:id element. This
implements Mark Pilgrom's tag: URI method, using the
CREATION-DATE of the entry, and the domain part of LINK"
    (format "tag:%s,%s:/%s"
	    (url-host (url-generic-parse-url link))
	    (format-time-string "%Y-%m-%d" creation-date)
	    (format-time-string "%Y%m%d%H%M%S" creation-date)))

(provide 'atom)
;;; atom.el ends here
