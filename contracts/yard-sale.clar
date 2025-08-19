;; yard-sale-coordinator.clar
;; Multi-family garage sale organization with item pooling and profit splitting

(define-constant ERR_NOT_AUTHORIZED (err u100))
(define-constant ERR_ITEM_NOT_FOUND (err u101))
(define-constant ERR_ITEM_SOLD (err u102))
(define-constant ERR_INVALID_PRICE (err u103))
(define-constant ERR_SALE_NOT_ACTIVE (err u104))

(define-data-var sale-active bool false)
(define-data-var next-item-id uint u1)

(define-map items
  { id: uint }
  {
    owner: principal,
    title: (string-ascii 50),
    description: (string-ascii 200),
    suggested-price: uint,
    actual-price: (optional uint),
    sold: bool,
    buyer: (optional principal),
    created-at: uint
  })

(define-map family-totals
  { owner: principal }
  { total-earned: uint })

(define-public (start-sale)
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
    (var-set sale-active true)
    (ok true)))

(define-public (end-sale)
  (begin
    (asserts! (is-eq tx-sender (var-get contract-owner)) ERR_NOT_AUTHORIZED)
    (var-set sale-active false)
    (ok true)))

(define-public (add-item (title (string-ascii 50)) (description (string-ascii 200)) (suggested-price uint))
  (let ((item-id (var-get next-item-id)))
    (asserts! (> suggested-price u0) ERR_INVALID_PRICE)
    (map-set items
      { id: item-id }
      {
        owner: tx-sender,
        title: title,
        description: description,
        suggested-price: suggested-price,
        actual-price: none,
        sold: false,
        buyer: none,
        created-at: stacks-block-height
      })
    (var-set next-item-id (+ item-id u1))
    (ok item-id)))

(define-public (mark-sold (item-id uint) (actual-price uint) (buyer principal))
  (let ((item (unwrap! (map-get? items { id: item-id }) ERR_ITEM_NOT_FOUND)))
    (asserts! (var-get sale-active) ERR_SALE_NOT_ACTIVE)
    (asserts! (not (get sold item)) ERR_ITEM_SOLD)
    (asserts! (> actual-price u0) ERR_INVALID_PRICE)

    (map-set items
      { id: item-id }
      (merge item {
        actual-price: (some actual-price),
        sold: true,
        buyer: (some buyer)
      }))

    (let ((current-total (default-to u0 (get total-earned (map-get? family-totals { owner: (get owner item) })))))
      (map-set family-totals
        { owner: (get owner item) }
        { total-earned: (+ current-total actual-price) }))

    (ok true)))

(define-read-only (get-item (item-id uint))
  (map-get? items { id: item-id }))

(define-read-only (get-family-earnings (owner principal))
  (default-to u0 (get total-earned (map-get? family-totals { owner: owner }))))

(define-read-only (is-sale-active)
  (var-get sale-active))

(define-data-var contract-owner principal tx-sender)
