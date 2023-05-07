(define-constant CONTRACT_ADDRESS (as-contract tx-sender))

(define-constant ERR_TITLE_DESCRIPTION_LINK_EMPTY (err u101))
(define-constant ERR_INVALID_FUND_GOAL (err u102))
(define-constant ERR_START_NOT_VALID (err u103))
(define-constant ERR_END_NOT_VALID (err u104))
(define-constant ERR_ID_NOT_FOUND (err u105))
(define-constant ERR_CANNOT_CANCEL (err u106))
(define-constant ERR_NOT_OWNER (err u107))
(define-constant ERR_NOT_STARTED (err u108))
(define-constant ERR_ENDED (err u109))
(define-constant ERR_PLEDGE_GREATER_THAN_ZERO (err u110))
(define-constant ERR_STX_TRANSFER_FAILED (err u111))
(define-constant ERR_NOT_PLEDGED (err u112))
(define-constant ERR_INVALID_UNPLEDGE_AMT (err u113))
(define-constant ERR_NOT_ENDED (err u114))
(define-constant ERR_GOAL_NOT_MET (err u115))
(define-constant ERR_ALREADY_CLAIMED (err u116))
(define-constant ERR_TARGET_NOT_REACHED (err u117))

;; calculate roughly 90 days based on block times of 10 minutes
(define-constant FUNDING_TIME_LIMIT u12960)

;; min amount to receive an NFT when pledging
(define-constant MIN_PLEDGE_AMOUNT_TO_RECEIVE_NTF u500)

(define-data-var last-id uint u0)

(define-map Campaigns uint {
    title: (string-utf8 256),
    description: (buff 33),
    link: (string-utf8 256),
    fundGoal: uint,
    startsAt: uint,
    endsAt: uint,
    campaignOwner: principal,
    pledgedCount: uint,
    pledgedAmount: uint,
    claimed: bool,
    targetReached: bool,
    targetReachedBy: uint
})

(define-map Investments {contributor: principal, campaignId: uint} {amount: uint})

(define-read-only (get-campaign (id uint))
    (ok (unwrap! (map-get? Campaigns id) ERR_ID_NOT_FOUND))
)

(define-read-only (get-investment (id uint) (contributor principal))
    (ok (map-get? Investments {contributor: contributor, campaignId: id}))
)

(define-public (launch (title (string-utf8 256)) (description (buff 33)) (link (string-utf8 256)) (fundGoal uint) (startsAt uint) (endsAt uint))
    (let
        (
            (id (+ (var-get last-id) u1))
        )
        (asserts! (> fundGoal u0) ERR_INVALID_FUND_GOAL)
        (asserts! (and (> (len title) u0) (> (len description) u0) (> (len link) u0)) ERR_TITLE_DESCRIPTION_LINK_EMPTY)
        (asserts! (>= startsAt block-height) ERR_START_NOT_VALID)
        (asserts! (>= endsAt block-height) ERR_END_NOT_VALID)
        (asserts! (<= (- endsAt startsAt) FUNDING_TIME_LIMIT) ERR_END_NOT_VALID)
        (map-set Campaigns id {
            title: title,
            description: description,
            link: link,
            fundGoal: fundGoal,
            startsAt: startsAt,
            endsAt: endsAt,
            campaignOwner: tx-sender,
            pledgedCount: u0,
            pledgedAmount: u0,
            claimed: false,
            targetReached: false,
            targetReachedBy: u0
        })
        (var-set last-id id)
        (ok id)
    )
)

(define-public (cancel (id uint))
    (let
        (
            (campaign (try! (get-campaign id)))
        )
        (asserts! (> (get startsAt campaign) block-height) ERR_CANNOT_CANCEL)
        (asserts! (is-eq (get campaignOwner campaign) tx-sender) ERR_NOT_OWNER)
        (ok (map-delete Campaigns id))
    )
)

(define-public (update (id uint) (title (string-utf8 256)) (description (buff 33)) (link (string-utf8 256)))
    (let
        (
            (campaign (try! (get-campaign id)))
        )
        (asserts! (is-eq (get campaignOwner campaign) tx-sender) ERR_NOT_OWNER)
        (asserts! (and (> (len title) u0) (> (len description) u0) (> (len link) u0)) ERR_TITLE_DESCRIPTION_LINK_EMPTY)
        (asserts! (> (get endsAt campaign) block-height) ERR_ENDED)
        (ok (map-set Campaigns id (merge campaign {
            title: title,
            description: description,
            link: link
        })))
    )
)

(define-public (claim (id uint))
    (let
        (
            (campaignOwner tx-sender)
            (campaign (try! (get-campaign id)))
            (pledgedAmount (get pledgedAmount campaign))
        )
        (asserts! (is-eq (get campaignOwner campaign) campaignOwner) ERR_NOT_OWNER)
        (asserts! (get targetReached campaign) ERR_TARGET_NOT_REACHED)
        (asserts! (not (get claimed campaign)) ERR_ALREADY_CLAIMED)
        (try! (as-contract (stx-transfer? pledgedAmount tx-sender campaignOwner)))
        (ok (map-set Campaigns id (merge campaign {
            claimed: true,
            targetReachedBy: block-height
        })))
    )
)

(define-public (pledge (id uint) (amount uint))
    (let
        (
            (campaign (try! (get-campaign id)))
            (contributor tx-sender)
            (investmentKey {contributor: contributor, campaignId: id})
            (investment (map-get? Investments investmentKey))
            (pledgedCount (get pledgedCount campaign))
            (pledgedAmount (+ (get pledgedAmount campaign) amount))
        )
        (asserts! (< (get startsAt campaign) block-height) ERR_NOT_STARTED)
        (asserts! (> (get endsAt campaign) block-height) ERR_ENDED)
        (asserts! (> amount u0) ERR_PLEDGE_GREATER_THAN_ZERO)
        (try! (stx-transfer? amount contributor CONTRACT_ADDRESS))
        (if (>= amount MIN_PLEDGE_AMOUNT_TO_RECEIVE_NTF)
            (try! (as-contract (contract-call? .donorpass mint contributor))) u0
        )
        (map-set Investments investmentKey {amount: (match investment
            existing (+ (get amount existing) amount)
            amount
        )})
        (map-set Campaigns id (merge campaign {
            pledgedAmount: pledgedAmount,
            pledgedCount: (if (is-none investment) (+ pledgedCount u1) pledgedCount),
            targetReached: (>= pledgedAmount (get fundGoal campaign)),
        }))
        (ok true)
    )
)

(define-public (unpledge (id uint) (amount uint))
    (let
        (
            (contributor tx-sender)
            (campaign (try! (get-campaign id)))
            (pledgedCount (get pledgedCount campaign))
            (investmentKey {contributor: contributor, campaignId: id})
            (investment (unwrap! (map-get? Investments investmentKey) ERR_NOT_PLEDGED))
            (investmentAmount (get amount investment))
            (pledgedAmount (get pledgedAmount campaign))
        )
        (asserts! (> (get endsAt campaign) block-height) ERR_ENDED)
        (asserts! (<= amount investmentAmount) ERR_INVALID_UNPLEDGE_AMT)
        (try! (as-contract (stx-transfer? amount tx-sender contributor)))
        (map-set Investments investmentKey {amount: (- investmentAmount amount)})
        (map-set Campaigns id (merge campaign {
            pledgedAmount: (- pledgedAmount amount),
            pledgedCount: (if (is-eq investmentAmount amount) (- pledgedCount u1) pledgedCount),
            targetReached: (>= (- pledgedAmount amount) (get fundGoal campaign))
        }))
        (ok true)
    )
)

(define-public (refund (id uint))
    (let
        (
            (campaign (try! (get-campaign id)))
            (contributor tx-sender)
            (investmentKey {contributor: contributor, campaignId: id})
            (investment (unwrap! (map-get? Investments investmentKey) ERR_NOT_PLEDGED))
        )
        (try! (as-contract (stx-transfer? (get amount investment) tx-sender contributor)))
        (asserts! (< (get endsAt campaign) block-height) ERR_NOT_ENDED)
        (asserts! (< (get pledgedAmount campaign) (get fundGoal campaign)) ERR_GOAL_NOT_MET)
        (ok (map-delete Investments investmentKey))
    )
)
