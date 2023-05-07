(impl-trait 'SP2PABAF9FTAJYNFZH93XENAJ8FVY99RRM50D2JG9.nft-trait.nft-trait)

(define-constant CLEARFUND_CONTRACT .clearfund)
(define-constant ERR_CLEARFUND_ONLY (err u100))
(define-constant ERR_NOT_TOKEN_OWNER (err u101))

(define-non-fungible-token donorpass uint)

(define-data-var lastTokenId uint u0)

(define-read-only (get-last-token-id)
    (ok (var-get lastTokenId))
)

(define-read-only (get-token-uri (id uint)) 
    (ok none)
)

(define-read-only (get-owner (id uint))
    (ok (nft-get-owner? donorpass id))
)

(define-public (transfer (id uint) (sender principal) (recipient principal))
    (begin
        (asserts! (is-eq tx-sender sender) ERR_NOT_TOKEN_OWNER)
        ;; #[filter(id, recipient)]
        (nft-transfer? donorpass id sender recipient)
    )
)

(define-public (mint (recipient principal))
    (let
        (
            (id (+ (var-get lastTokenId) u1))
        )
        (asserts! (is-eq CLEARFUND_CONTRACT tx-sender) ERR_CLEARFUND_ONLY)
        ;; #[filter(recipient)]
        (try! (nft-mint? donorpass id recipient))
        (var-set lastTokenId id)
        (ok id)
    )
)
