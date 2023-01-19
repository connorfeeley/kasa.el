;;; kasa.el --- Control TP-Link Kasa smart home devices  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Connor Feeley
;;
;; Author: Connor Feeley <git@cfeeley.org>
;; Maintainer: Connor Feeley <git@cfeeley.org>
;; Created: December 8, 2022
;; Modified: December 10, 2022
;; Version: 0.2.1
;; Keywords: kasa, tools, unix, hardware
;; Homepage: https://sr.ht/~cfeeley/kasa.el/
;; Package-Requires: ((emacs "26.1"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Description:
;;
;; This package provides an interface between Emacs and the python-kasa library
;; for controlling TP-Link Kasa smart home devices.
;;
;;; License:
;;
;; Copyright (c) 2022 Connor Feeley
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
;; BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
;; ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;; Code:

;;; ----------------------------------------------------------------------------
;;; Customization options
;;; ----------------------------------------------------------------------------
(defgroup kasa nil
  "Control TP-Link Kasa smart home devices."
  :prefix "kasa-"
  :group 'hardware)
(defcustom kasa-python-executable-name "kasa"
  "The executable to use (provided by python-kasa)."
  :type 'string
  :group 'kasa)
(defcustom kasa-enable-tramp t
  "If `kasa-python-executable-name' should be run on the remote hosts.
The remote host is determined by `default-directory'."
  :type 'boolean
  :group 'kasa)
(defcustom kasa-default-target
  '(:host "192.168.0.240"
    :device-type "strip"
    :name "Ultrix-2NS")
  "Default target to operate on."
  :type '(plist :options ((:host (string :tag "Host"))
                          (:device-type (string :tag "Type"))
                          (:name (string :tag "Name")))
          :key-type (sexp :tag "Key")
          :value-type (string :tag "Description"))
  :group 'kasa)

;;; ----------------------------------------------------------------------------
;;; Core functions
;;; ----------------------------------------------------------------------------
(defun kasa--executable ()
  "Get the kasa executable to use, defined by `kasa-python-executable-name'.
If `kasa-enable-tramp' is non-nil, and `default-directory' is on a remote host,
then the executable will be run on that host."
  (executable-find kasa-python-executable-name kasa-enable-tramp))

(defun kasa--run (args)
  "Run python-kasa with ARGS."
  (apply #'process-lines (kasa--executable) args))

(defun kasa-toggle-default (&optional state)
  "Toggle the power state of the default target.
If STATE is non-nil, turn the target on. If STATE is nil, turn the target off."
  (interactive)
  (let* ((host (plist-get kasa-default-target :host))
         (type (plist-get kasa-default-target :device-type))
         (name (plist-get kasa-default-target :name))
         (state-string (cond ((not state) "toggle")
                             ((eq state 'on) "on")
                             ((eq state 'off) "off"))))
    (kasa--run (list
                "--host" host
                "--type" type
                state-string
                "--name" name))))

;;; ----------------------------------------------------------------------------
;;; Interactive convenience functions
;;; ----------------------------------------------------------------------------
(defmacro kasa-on-default () `(kasa-toggle-default t) "Turn the default target on." )
(defmacro kasa-off-default () `(kasa-toggle-default nil) "Turn the default target off." )

(defun kasa-cycle-default ()
  "Cycle the power state of the default target (off, then on)."
  (interactive)
  (kasa-on-default)
  (kasa-off-default))

;;; ----------------------------------------------------------------------------
(provide 'kasa)
;;; kasa.el ends here
