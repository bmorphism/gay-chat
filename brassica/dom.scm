;;; Copyright (C) 2025 David Thompson <dave@spritely.institute>
;;;
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;;
;;;    http://www.apache.org/licenses/LICENSE-2.0
;;;
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

;;; Commentary:
;;;
;;; DOM helpers.
;;;
;;; Code:

(define-module (brassica dom)
  #:use-module (brassica dom document)
  #:use-module (brassica dom element)
  #:use-module (brassica dom event)
  #:use-module (hoot ffi)
  #:use-module (ice-9 match)
  #:export (remove-all-children!
            replace-children!
            set-inner-shtml!
            set-outer-shtml!
            shtml->dom))

(define (remove-all-children! elem)
  (let lp ((elem (first-child elem)))
    (unless (external-null? elem)
      (let ((next (next-sibling elem)))
        (remove! elem)
        (lp next)))))

(define (replace-children! elem children)
  (remove-all-children! elem)
  (for-each (lambda (child) (append-child! elem child)) children))

(define (set-inner-shtml! container shtml)
  (remove-all-children! container)
  (match shtml
    ((or (? string? shtml) ((? symbol?) . _))
     (append-child! container (shtml->dom shtml)))
    (_
     (for-each (lambda (shtml)
                 (append-child! container (shtml->dom shtml)))
               shtml))))

(define (set-outer-shtml! elem shtml)
  (replace-with! elem (shtml->dom shtml)))

(define (shtml->dom shtml)
  (match shtml
    ;; The simple case: a string representing a text node.
    ((? string? str)
     (make-text-node str))
    ((? number? num)
     (make-text-node (number->string num)))
    ;; An element tree.  The first item is the HTML tag.
    (((? symbol? tag) . body)
     ;; Create a new element with the given tag.
     (let ((elem (make-element (symbol->string tag))))
       (define (add-children children)
         ;; Recursively call shtml->dom for each child node and
         ;; append it to elem.
         (for-each (lambda (child)
                     (append-child! elem (shtml->dom child)))
                   children))
       (match body
         (() (values))
         ((('@ . attrs) . children)
          (for-each
           (lambda (attr)
             (match attr
               (((? symbol? name) (? string? val))
                (set-attribute! elem
                                (symbol->string name)
                                val))
               (((? symbol? name) (? number? val))
                (set-attribute! elem
                                (symbol->string name)
                                (number->string val)))
               (((? symbol? name) (? procedure? proc))
                (add-event-listener! elem
                                     (symbol->string name)
                                     (procedure->external proc)))))
           attrs)
          (add-children children))
         (children (add-children children)))
       elem))))
