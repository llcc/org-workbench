;;; org-workbench.el --- Digital card workbench for org-mode -*- lexical-binding: t -*-

;; Copyright (C) 2025 Yibie

;; Author: Yibie <yibie@outlook.com>
;; Maintainer: Yibie <yibie@outlook.com>
;; URL: https://github.com/yibie/org-workbench
;; Version: 0.1.0
;; Package-Requires: ((emacs "28.1") (org "9.6"))
;; Keywords: org-mode, workbench, card-system, note-taking

;; This file is not part of GNU Emacs.

;; The MIT License (MIT)

;; Copyright (c) 2024 Yibie Permission is hereby granted, free of
;; charge, to any person obtaining a copy of this software and
;; associated documentation files (the "Software"), to deal in the
;; Software without restriction, including without limitation the
;; rights to use, copy, modify, merge, publish, distribute,
;; sublicense, and/or sell copies of the Software, and to permit
;; persons to whom the Software is furnished to do so, subject to the
;; following conditions: The above copyright notice and this
;; permission notice shall be included in all copies or substantial
;; portions of the Software.  The Software is provided "as is",
;; without warranty of any kind, express or implied, including but not
;; limited to the warranties of merchantability, fitness for a
;; particular purpose and noninfringement. In no event shall the
;; authors or copyright holders be liable for any claim, damages or
;; other liability, whether in an action of contract, tort or
;; otherwise, arising from, out of or in connection with the software
;; or the use or other dealings in the Software.

;;; Commentary:

;; This package provides a digital card workbench system for org-mode.
;; It allows you to create, organize, and manage cards from your org-mode notes.
;; Perfect for research organization, writing projects, and argument structure building.

;;; Code:

(require 'org)
(require 'cl-lib)

(defgroup org-workbench nil
  "Digital card workbench for org-mode."
  :group 'org
  :prefix "org-workbench-")

;; Configuration options
(defcustom org-workbench-save-file 
  (expand-file-name "org-workbench-db.el" user-emacs-directory)
  "File to save all workbenches state across sessions."
  :type 'file
  :group 'org-workbench)

(defcustom org-workbench-buffer-name "*Org Workbench*"
  "Name of the workbench buffer."
  :type 'string
  :group 'org-workbench)

;; Internal variables
(defvar org-workbench-workbenches (make-hash-table :test 'equal)
  "Hash table of workbenches. Key is workbench name, value is card list.")

(defvar org-workbench-current-workbench "default"
  "Name of currently active workbench.")

;;------------------------------------------------------------------------------
;; Key Bindings
;;------------------------------------------------------------------------------

(defvar org-workbench-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c w h") 'org-workbench-add-heading)
    (define-key map (kbd "C-c w m") 'org-workbench-manage)
    map)
  "Keymap for org-workbench-mode.")

;;------------------------------------------------------------------------------
;; Core Functions
;;------------------------------------------------------------------------------

(defun org-workbench--get-or-create-id ()
  "Get existing ID or create a new one if ID system is enabled."
  (or (org-entry-get (point) "ID")
      (progn
        (org-id-get-create)
        (org-entry-get (point) "ID"))))

(defun org-workbench--heading-content ()
  "Extracts the direct content of the current heading, stopping at the next
heading or the end of the buffer."
  (save-excursion
    (let ((content (save-restriction
                     (org-narrow-to-subtree)
                     (buffer-substring (point-min) (point-max))))
          (dir (org-attach-dir)))
      (with-temp-buffer
        (org-mode)
        (insert content)
        (goto-char (point-min))
        (org-delete-property "DIR")
        (org-set-property "DIR" dir)
        (let ((headline (org-element-at-point)))
          (buffer-substring
           (org-element-property :contents-begin headline)
           (org-element-property :contents-end headline)))))))

(defun org-workbench--get-cards ()
  "Get cards from current workbench."
  (gethash org-workbench-current-workbench org-workbench-workbenches nil))

(defun org-workbench--set-cards (cards)
  "Set cards for current workbench."
  (puthash org-workbench-current-workbench cards org-workbench-workbenches))

(defun org-workbench--ensure-default-workbench ()
  "Ensure default workbench exists."
  (unless (gethash "default" org-workbench-workbenches)
    (puthash "default" nil org-workbench-workbenches)))

(defun org-workbench--extract-card-info ()
  "Extract card information from heading."
  (let* ((title (org-get-heading t t t t))
         (level (org-current-level))
         (file (buffer-file-name))
         (id (org-entry-get (point) "ID"))
         (content (org-workbench--heading-content)))
    (list :id id 
          :title title
          :content content
          :level level
          :file file)))

;;------------------------------------------------------------------------------
;; Workbench Management
;;------------------------------------------------------------------------------

(defun org-workbench-create ()
  "Create a new workbench."
  (interactive)
  (let ((name (read-string "New workbench name: ")))
    (when name
      (puthash name nil org-workbench-workbenches)
      (setq org-workbench-current-workbench name)
      (org-workbench--save)
      (org-workbench-show)
      (message "Created and switched to workbench: %s" name))))

(defun org-workbench-manage ()
  "Manage workbenches (rename, delete)."
  (interactive)
  (let* ((workbench-names (hash-table-keys org-workbench-workbenches))
         (workbench-choices (mapcar (lambda (name)
                                      (format "%s%s (%d cards)"
                                              (if (string= name org-workbench-current-workbench) "[Current] " "")
                                              name
                                              (length (gethash name org-workbench-workbenches))))
                                    workbench-names))
         (choices (append workbench-choices
                          '("+ Create new workbench..."
                            "- Delete workbench..."
                            "✏️ Rename workbench...")))
         (choice (completing-read "Manage workbench: " choices nil t)))
    (cond
     ((string= choice "+ Create new workbench...")
      (org-workbench-create))
     ((string= choice "-️ Delete workbench...")
      (org-workbench-delete))
     ((string= choice "✏️ Rename workbench...")
      (org-workbench-rename))
     (t
      ;; Extract workbench name from choice
      (let ((name (car (split-string choice " ("))))
        (setq org-workbench-current-workbench name)
        (org-workbench-show)
        (message "Switched to workbench: %s" name))))))

(defun org-workbench-delete ()
  "Delete a workbench."
  (interactive)
  (let* ((workbench-names (hash-table-keys org-workbench-workbenches))
         (choices (mapcar (lambda (name)
                            (format "%s (%d cards)"
                                    name
                                    (length (gethash name org-workbench-workbenches))))
                          workbench-names))
         (choice (completing-read "Delete workbench: " choices nil t)))
    (when choice
      (let ((name (car (split-string choice " ("))))
        (when (and (not (string= name "default"))
                   (y-or-n-p (format "Delete workbench '%s' with %d cards? "
                                     name (length (gethash name org-workbench-workbenches)))))
          (remhash name org-workbench-workbenches)
          (when (string= name org-workbench-current-workbench)
            (setq org-workbench-current-workbench "default"))
          (org-workbench--save)
          (org-workbench-show)
          (message "Deleted workbench: %s" name))))))

(defun org-workbench-rename ()
  "Rename a workbench."
  (interactive)
  (let* ((workbench-names (hash-table-keys org-workbench-workbenches))
         (choices (mapcar (lambda (name)
                            (format "%s (%d cards)"
                                    name
                                    (length (gethash name org-workbench-workbenches))))
                          workbench-names))
         (choice (completing-read "Rename workbench: " choices nil t)))
    (when choice
      (let ((old-name (car (split-string choice " (")))
            (new-name (read-string "New name: ")))
        (when (and new-name (not (string= new-name "")))
          (let ((cards (gethash old-name org-workbench-workbenches)))
            (puthash new-name cards org-workbench-workbenches)
            (remhash old-name org-workbench-workbenches)
            (when (string= old-name org-workbench-current-workbench)
              (setq org-workbench-current-workbench new-name))
            (org-workbench--save)
            (org-workbench-show)
            (message "Renamed workbench from '%s' to '%s'" old-name new-name)))))))

;;------------------------------------------------------------------------------
;; Card Operations
;;------------------------------------------------------------------------------

(defun org-workbench-add-heading ()
  "Add only the current heading (without subtree) to current workbench."
  (interactive) 
  (unless (derived-mode-p 'org-mode)
    (user-error "Not in an org-mode buffer"))
  
  (org-workbench--ensure-default-workbench)
  (save-excursion
    (org-back-to-heading t)
    (let* ((title (org-get-heading t t t t))
           (file (buffer-file-name))
           (level (org-current-level))
           (content (org-workbench--heading-content)))
      
      (let* ((id (org-workbench--get-or-create-id))
             (card-info (list :id id
                              :title title
                              :content content
                              :level level
                              :file file))
             (current-cards (org-workbench--get-cards)))
        
        (unless (catch 'found
                  (dolist (card current-cards)
                    (when (or (and id (equal (plist-get card :id) id))
                              (equal (plist-get card :title) title))
                      (throw 'found t)))
                  nil)
          (org-workbench--set-cards (cons card-info current-cards))
          (org-workbench--save)
          (message "Added heading: %s to workbench: %s" title org-workbench-current-workbench)
          
          ;; Automatically refresh workbench display
          (let ((workbench-buffer (get-buffer org-workbench-buffer-name)))
            (when workbench-buffer
              (org-workbench-show))))))))

;;------------------------------------------------------------------------------
;; Display Functions
;;------------------------------------------------------------------------------

(defun org-workbench-show ()
  "Display workbench as clean org-mode outline with all cards as top-level
headings."
  (interactive)
  (let* ((workbench-name org-workbench-current-workbench)
         (buffer-name org-workbench-buffer-name)
         (buffer (get-buffer-create buffer-name)))
    (with-current-buffer buffer
      (erase-buffer)
      (org-mode)
      
      ;; Configure display settings
      (setq-local org-hide-leading-stars t)
      (setq-local org-startup-folded t)
      (setq-local org-adapt-indentation nil)
      (org-num-mode t)
      
      ;; Title with ID system status
      (insert (format "#+title:%s (%d cards)" 
                      workbench-name 
                      (length (org-workbench--get-cards))))
      (insert "\n\n")
      
      ;; Render cards
      (let ((cards (org-workbench--get-cards)))
        (if (null cards)
            (insert "Workbench is empty.\n\nM-x org-workbench-add-subtree/M-x org-workbench-add-heading to add cards.")
          (dolist (card cards)
            (let* ((start (point))
                   (title (plist-get card :title))
                   (content (plist-get card :content)))
              
              ;; Force all cards to be displayed as top-level headings, regardless of their original level
              (insert (format "* %s\n" title))
              
              ;; Insert content
              (when (and content (not (string= content "")))
                (insert content)
                (insert "\n"))
              
              (insert "\n")
              
              ;; Store complete card information as text properties
              (put-text-property start (point) 'workbench-card card)))))
      
      ;; Enable workbench mode
      (org-workbench-org-mode 1)
      (font-lock-flush)
      (goto-char (point-min))
      
      ;; Jump to the first card
      (when (org-workbench--get-cards)
        (re-search-forward "^\\* " nil t)
        (beginning-of-line)))
    
    (display-buffer buffer)
    (select-window (get-buffer-window buffer))))

;;------------------------------------------------------------------------------
;; Persistence
;;------------------------------------------------------------------------------

(defun org-workbench--save ()
  "Save workbenches to file."
  (with-temp-buffer
    (insert ";; -*- mode: emacs-lisp; lexical-binding: t -*-\n")
    (insert ";; Org Workbench Data - Auto-generated\n\n")
    (insert (format "(setq org-workbench-workbenches %S)\n" org-workbench-workbenches))
    (insert (format "(setq org-workbench-current-workbench %S)\n" org-workbench-current-workbench))
    (write-file org-workbench-save-file)))

(defun org-workbench--load ()
  "Load workbenches from file."
  (when (file-exists-p org-workbench-save-file)
    (load org-workbench-save-file nil t)))

;;------------------------------------------------------------------------------
;; Card Operations
;;------------------------------------------------------------------------------

(defun org-workbench-get-current-card ()
  "Get card info at current heading."
  (when (org-at-heading-p)
    (get-text-property (point) 'workbench-card)))

(defun org-workbench-remove-card ()
  "Remove current card from workbench."
  (interactive)
  (let ((card (org-workbench-get-current-card)))
    (if card
        (when (y-or-n-p (format "Remove card: %s? " (plist-get card :title)))
          (let ((id (plist-get card :id))
                (current-cards (org-workbench--get-cards)))
            (setq current-cards 
                  (delq nil (mapcar (lambda (c) 
                                      (unless (and id (plist-get c :id))
                                        c))
                                    current-cards)))
            (org-workbench--set-cards current-cards)
            (org-workbench--save)
            
            ;; Remove from display
            (org-cut-subtree)
            
            ;; Update the workbench display to refresh the card counter
            (org-workbench-show)
            (message "Removed card: %s" (plist-get card :title))))
      (user-error "Not on a workbench card"))))

(defun org-workbench-save-order ()
  "Save current card order to workbench."
  (interactive)
  (let ((new-order '()))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^\\* " nil t)
        (let ((card (get-text-property (point) 'workbench-card)))
          (when card
            (push card new-order)))))
    
    (when new-order
      (org-workbench--set-cards (nreverse new-order))
      (org-workbench--save)
      (message "Saved card order for workbench: %s" org-workbench-current-workbench))))

(defun org-workbench-clear ()
  "Clear all cards from current workbench."
  (interactive)
  (when (y-or-n-p (format "Clear all cards from workbench '%s'? " org-workbench-current-workbench))
    (org-workbench--set-cards nil)
    (org-workbench--save)
    (org-workbench-show)
    (message "Workbench '%s' cleared" org-workbench-current-workbench)))

;;------------------------------------------------------------------------------
;; ID-based Enhanced Functions
;;------------------------------------------------------------------------------

(defun org-workbench-goto-source ()
  "Jump to the source location of the current card.
This function requires ID system to be enabled and the card to have an ID."
  (interactive)
  (let* ((card (org-workbench-get-current-card))
         (id (when card (plist-get card :id))))
    (org-id-goto id)
    (message "Jumped to source: %s" (plist-get card :title))))

(defun org-workbench-sync-card ()
  "Sync the content of the current card with its source.
This function requires ID system to be enabled and the card to have an ID."
  (interactive)
  (let* ((card (org-workbench-get-current-card))
         (id (when card (plist-get card :id))))
    (message "Syncing card %s..." id)
    (let* ((current-cards (org-workbench--get-cards))
           (card-index (catch 'found
                         (let ((index 0))
                           (dolist (c current-cards)
                             (when (equal c card)
                               (throw 'found index))
                             (setq index (1+ index)))
                           nil)))
           (updated-card (when card-index
                           (save-excursion
                             (org-id-goto id)
                             (org-workbench--extract-card-info)))))
      (if updated-card
          (progn
            ;; Replace the old card with the updated one
            (setf (nth card-index current-cards) updated-card)
            (org-workbench--set-cards current-cards)
            (org-workbench--save)
            (org-workbench-show)
            (message "Card synced successfully"))
        (message "Failed to sync card: could not retrieve updated data")))))

(defun org-workbench-sync-all-cards ()
  "Sync all cards in the current workbench with their sources.
This function requires ID system to be enabled."
  (interactive)
  
  (let* ((current-cards (org-workbench--get-cards))
         (total-cards (length current-cards))
         (synced-count 0))
    (if (zerop total-cards)
        (message "No cards to sync")
      (progn
        (message "Syncing %d cards..." total-cards)
        (dolist (card current-cards)
          (let* ((id (plist-get card :id))
                 (updated-card (when id
                                 (save-excursion
                                   (org-id-goto id)
                                   (org-workbench--extract-card-info)))))
            (when updated-card
              (setq current-cards (mapcar (lambda (c)
                                            (if (equal c card) updated-card c))
                                          current-cards))
              (setq synced-count (1+ synced-count)))))
        (org-workbench--set-cards current-cards)
        (org-workbench--save)
        (org-workbench-show)
        (message "Synced %d/%d cards successfully" synced-count total-cards)))))

(defun org-workbench-move-up ()
  "Move current card up and automatically save order."
  (interactive)
  (let ((buffer (get-buffer org-workbench-buffer-name)))
    (when buffer
      (with-current-buffer buffer
        (let ((was-read-only buffer-read-only))
          (setq buffer-read-only nil)
          (org-move-subtree-up)
          (setq buffer-read-only was-read-only)
          ;; Automatically save the new order
          (org-workbench-save-order))))))

(defun org-workbench-move-down ()
  "Move current card down and automatically save order."
  (interactive)
  (let ((buffer (get-buffer org-workbench-buffer-name)))
    (when buffer
      (with-current-buffer buffer
        (let ((was-read-only buffer-read-only))
          (setq buffer-read-only nil)
          (org-move-subtree-down)
          (setq buffer-read-only was-read-only)
          ;; Automatically save the new order
          (org-workbench-save-order))))))

;;------------------------------------------------------------------------------
;; Mode Definitions
;;------------------------------------------------------------------------------

(define-minor-mode org-workbench-mode
  "Minor mode for org-workbench."
  :lighter " Workbench"
  :keymap org-workbench-mode-map)

;; Workbench org-mode
(defvar org-workbench-org-mode-map
  (let ((map (make-sparse-keymap)))
    (set-keymap-parent map org-mode-map)
    
    ;; Workbench specific functions
    (define-key map (kbd "C-c w k") 'org-workbench-remove-card)
    (define-key map (kbd "C-c w s") 'org-workbench-save-order)
    (define-key map (kbd "g") 'org-workbench-show)
    
    ;; ID-based enhanced functions (only available when ID system is enabled)
    (define-key map (kbd "RET") 'org-workbench-goto-source)
    (define-key map (kbd "C-c w c") 'org-workbench-sync-card)
    (define-key map (kbd "C-c w a") 'org-workbench-sync-all-cards)
    
    ;; Movement with auto-save
    (define-key map (kbd "M-<up>") 'org-workbench-move-up)
    (define-key map (kbd "M-<down>") 'org-workbench-move-down)
    (define-key map (kbd "M-p") 'org-workbench-move-up)
    (define-key map (kbd "M-n") 'org-workbench-move-down)
    
    ;; Navigation
    (define-key map (kbd "n") 'org-next-visible-heading)
    (define-key map (kbd "p") 'org-previous-visible-heading)
    map)
  "Keymap for org-workbench workbench in org-mode.")

(define-minor-mode org-workbench-org-mode
  "Minor mode for org-workbench workbench in org-mode."
  :lighter " Workbench"
  :keymap org-workbench-org-mode-map
  (when org-workbench-org-mode
    (add-hook 'org-after-refile-insert-hook 'org-workbench-save-order nil t)))

;;------------------------------------------------------------------------------
;; Setup and Cleanup
;;------------------------------------------------------------------------------

(defun org-workbench-setup ()
  "Setup org-workbench."
  (org-workbench--load)
  (org-workbench--ensure-default-workbench))

;; Ensure setup is run when org-mode is loaded
(eval-after-load 'org
  '(org-workbench-setup))

;; Also ensure setup is run when org-workbench is loaded
(eval-after-load 'org-workbench
  '(org-workbench-setup))

(provide 'org-workbench)
;;; org-workbench.el ends here 
