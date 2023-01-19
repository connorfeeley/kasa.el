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
(defgroup kasa nil "Control TP-Link Kasa smart home devices." :group 'hardware)
(defcustom kasa-python-executable "kasa"
  "The executable to use (provided by python-kasa)."
  :type 'string
  :group 'kasa)
(defcustom kasa-enable-tramp t
  "If `kasa-python-executable' should be run on the remote hosts.
The remote host is determined by `default-directory'."
  :type 'boolean
  :group 'kasa)
(defcustom kasa-default-target (list 'host "192.168.0.240" 'type "strip" 'plug "Ultrix-2NS")
  "Default target to operate on."
  :type 'plist
  :options '('host "string" 'type "string" 'plug "string")
  :group 'kasa)

;;; ----------------------------------------------------------------------------
;;; Core functions
;;; ----------------------------------------------------------------------------
(defun kasa--exec ()
  "Get the kasa executable to use, defined by `kasa-python-executable'.
If `kasa-enable-tramp' is non-nil, and `default-directory' is on a remote host,
then the executable will be run on that host."
  (executable-find kasa-python-executable kasa-enable-tramp))

(defun kasa--run (args)
  "Run python-kasa with ARGS."
  (apply #'process-lines (kasa--exec) args))

(defun kasa-toggle-default (state)
  "Toggle the power state of the default target.
If STATE is non-nil, turn the target on. Ff STATE is nil, turn the target off."
  (interactive)
  (let* ((host (plist-get kasa-default-target 'host))
         (type (plist-get kasa-default-target 'type))
         (plug (plist-get kasa-default-target 'plug)))
    (kasa--run (list "--host" host "--type" type (if state "on" "off") "--name" plug))))

;;; ----------------------------------------------------------------------------
;;; Interactive convenience functions
;;; ----------------------------------------------------------------------------
(defun kasa-on-default () "Turn the default target on." (interactive) (kasa-toggle-default t))

(defun kasa-off-default () "Turn the default target off." (interactive) (kasa-toggle-default nil))

(defun kasa-cycle-default ()
  "Cycle the power state of the default target (off, then on)."
  (interactive)
  (kasa-on-default)
  (kasa-off-default))

;;; ----------------------------------------------------------------------------
(provide 'kasa)
;;; kasa.el ends here
