(define-non-fungible-token art-nft uint)

(define-constant contract-owner tx-sender)
(define-constant min-auction-duration u100)
(define-constant max-auction-duration u10000)
(define-constant royalty-percentage u5)
(define-constant anti-snipe-blocks u12)

(define-data-var nft-counter uint u0)

(define-map auctions
    uint 
    {
        creator: principal,
        reserve-price: uint,
        end-block: uint,
        highest-bid: uint,
        highest-bidder: (optional principal),
        status: (string-ascii 20),
        extensions: uint
    }
)

(define-map bids
    { auction-id: uint, bidder: principal }
    uint
)

(define-map artist-royalties
    principal
    uint
)

(define-map bidder-amounts
    { auction-id: uint, bidder: principal }
    uint
)

(define-read-only (get-nft-owner (token-id uint))
    (nft-get-owner? art-nft token-id)
)

(define-read-only (get-auction (auction-id uint))
    (map-get? auctions auction-id)
)

(define-read-only (get-bid (auction-id uint) (bidder principal))
    (map-get? bids { auction-id: auction-id, bidder: bidder })
)

(define-read-only (get-bidder-amount (auction-id uint) (bidder principal))
    (default-to u0 (map-get? bidder-amounts { auction-id: auction-id, bidder: bidder }))
)

(define-read-only (get-artist-royalties (artist principal))
    (default-to u0 (map-get? artist-royalties artist))
)

(define-read-only (is-in-anti-snipe-period (auction-id uint))
    (match (map-get? auctions auction-id)
        auction (let ((blocks-remaining (- (get end-block auction) stacks-block-height)))
            (and 
                (is-eq (get status auction) "active")
                (<= blocks-remaining anti-snipe-blocks)
                (> blocks-remaining u0)
            )
        )
        false
    )
)

(define-read-only (get-auction-status (auction-id uint))
    (match (map-get? auctions auction-id)
        auction (get status auction)
        "not-found"
    )
)

(define-read-only (get-total-bids (auction-id uint))
    (match (map-get? auctions auction-id)
        auction (get highest-bid auction)
        u0
    )
)

(define-public (mint-nft)
    (let ((token-id (+ (var-get nft-counter) u1)))
        (try! (nft-mint? art-nft token-id tx-sender))
        (var-set nft-counter token-id)
        (ok token-id)
    )
)

(define-public (start-auction (token-id uint) (reserve-price uint) (duration uint))
    (let ((owner (unwrap! (nft-get-owner? art-nft token-id) (err u1))))
        (asserts! (is-eq tx-sender owner) (err u2))
        (asserts! (>= duration min-auction-duration) (err u3))
        (asserts! (<= duration max-auction-duration) (err u4))
        (asserts! (> reserve-price u0) (err u5))
        (try! (nft-transfer? art-nft token-id tx-sender (as-contract tx-sender)))
        (map-set auctions token-id {
            creator: tx-sender,
            reserve-price: reserve-price,
            end-block: (+ stacks-block-height duration),
            highest-bid: u0,
            highest-bidder: none,
            status: "active",
            extensions: u0
        })
        (ok true)
    )
)

(define-public (place-bid (auction-id uint) (bid-amount uint))
    (let (
        (auction (unwrap! (map-get? auctions auction-id) (err u6)))
        (current-bid (default-to u0 (map-get? bids { auction-id: auction-id, bidder: tx-sender })))
        (total-bid (+ current-bid bid-amount))
        (blocks-remaining (- (get end-block auction) stacks-block-height))
        (needs-extension (and (<= blocks-remaining anti-snipe-blocks) (> blocks-remaining u0)))
        (new-end-block (if needs-extension (+ stacks-block-height anti-snipe-blocks) (get end-block auction)))
        (new-extensions (if needs-extension (+ (get extensions auction) u1) (get extensions auction)))
    )
        (asserts! (is-eq (get status auction) "active") (err u7))
        (asserts! (> blocks-remaining u0) (err u8))
        (asserts! (> total-bid (get highest-bid auction)) (err u9))
        (asserts! (>= total-bid (get reserve-price auction)) (err u10))
        (try! (stx-transfer? bid-amount tx-sender (as-contract tx-sender)))
        (map-set bids { auction-id: auction-id, bidder: tx-sender } total-bid)
        (map-set auctions auction-id (merge auction {
            highest-bid: total-bid,
            highest-bidder: (some tx-sender),
            end-block: new-end-block,
            extensions: new-extensions
        }))
        (ok true)
    )
)

(define-public (place-bid-with-refund (auction-id uint) (bid-amount uint))
    (let (
        (auction (unwrap! (map-get? auctions auction-id) (err u11)))
        (current-bidder-amount (get-bidder-amount auction-id tx-sender))
        (total-bid (+ current-bidder-amount bid-amount))
        (previous-highest-bidder (get highest-bidder auction))
        (previous-highest-bid (get highest-bid auction))
        (blocks-remaining (- (get end-block auction) stacks-block-height))
        (needs-extension (and (<= blocks-remaining anti-snipe-blocks) (> blocks-remaining u0)))
        (new-end-block (if needs-extension (+ stacks-block-height anti-snipe-blocks) (get end-block auction)))
        (new-extensions (if needs-extension (+ (get extensions auction) u1) (get extensions auction)))
    )
        (asserts! (is-eq (get status auction) "active") (err u12))
        (asserts! (> blocks-remaining u0) (err u13))
        (asserts! (> total-bid previous-highest-bid) (err u14))
        (asserts! (>= total-bid (get reserve-price auction)) (err u15))
        
        (try! (stx-transfer? bid-amount tx-sender (as-contract tx-sender)))
        
        (match previous-highest-bidder
            prev-bidder (if (not (is-eq prev-bidder tx-sender))
                (try! (as-contract (stx-transfer? previous-highest-bid (as-contract tx-sender) prev-bidder)))
                true
            )
            true
        )
        
        (map-set bidder-amounts { auction-id: auction-id, bidder: tx-sender } total-bid)
        (map-set auctions auction-id (merge auction {
            highest-bid: total-bid,
            highest-bidder: (some tx-sender),
            end-block: new-end-block,
            extensions: new-extensions
        }))
        (ok true)
    )
)

(define-public (end-auction (auction-id uint))
    (let (
        (auction (unwrap! (map-get? auctions auction-id) (err u16)))
        (highest-bidder (get highest-bidder auction))
        (highest-bid (get highest-bid auction))
        (creator (get creator auction))
    )
        (asserts! (>= stacks-block-height (get end-block auction)) (err u17))
        (asserts! (is-eq (get status auction) "active") (err u18))
        
        (if (and (is-some highest-bidder) (>= highest-bid (get reserve-price auction)))
            (let (
                (royalty (/ (* highest-bid royalty-percentage) u100))
                (creator-payment (- highest-bid royalty))
                (winner (unwrap! highest-bidder (err u19)))
            )
                (try! (as-contract (nft-transfer? art-nft auction-id tx-sender winner)))
                (try! (as-contract (stx-transfer? creator-payment (as-contract tx-sender) creator)))
                (try! (as-contract (stx-transfer? royalty (as-contract tx-sender) creator)))
                (map-set artist-royalties creator (+ (get-artist-royalties creator) royalty))
            )
            (begin
                (try! (as-contract (nft-transfer? art-nft auction-id tx-sender creator)))
                (match highest-bidder
                    bidder (try! (as-contract (stx-transfer? highest-bid (as-contract tx-sender) bidder)))
                    true
                )
            )
        )
        
        (map-set auctions auction-id (merge auction { status: "ended" }))
        (ok true)
    )
)

(define-public (cancel-auction (auction-id uint))
    (let (
        (auction (unwrap! (map-get? auctions auction-id) (err u20)))
        (creator (get creator auction))
        (highest-bidder (get highest-bidder auction))
        (highest-bid (get highest-bid auction))
    )
        (asserts! (is-eq tx-sender creator) (err u21))
        (asserts! (is-eq (get status auction) "active") (err u22))
        (asserts! (is-eq highest-bid u0) (err u23))
        
        (try! (as-contract (nft-transfer? art-nft auction-id tx-sender creator)))
        (map-set auctions auction-id (merge auction { status: "cancelled" }))
        (ok true)
    )
)

(define-public (emergency-end-auction (auction-id uint))
    (let (
        (auction (unwrap! (map-get? auctions auction-id) (err u24)))
        (highest-bidder (get highest-bidder auction))
        (highest-bid (get highest-bid auction))
        (creator (get creator auction))
    )
        (asserts! (is-eq tx-sender contract-owner) (err u25))
        (asserts! (is-eq (get status auction) "active") (err u26))
        
        (try! (as-contract (nft-transfer? art-nft auction-id tx-sender creator)))
        (match highest-bidder
            bidder (try! (as-contract (stx-transfer? highest-bid (as-contract tx-sender) bidder)))
            true
        )
        (map-set auctions auction-id (merge auction { status: "emergency-ended" }))
        (ok true)
    )
)

(define-private (refund-bids (auction-id uint))
    (let ((auction (unwrap! (map-get? auctions auction-id) (err u27))))
        (match (get highest-bidder auction)
            bidder (try! (as-contract (stx-transfer? (get highest-bid auction) (as-contract tx-sender) bidder)))
            true
        )
        (ok true)
    )
)

(define-private (refund-all-bidders (auction-id uint))
    (let ((auction (unwrap! (map-get? auctions auction-id) (err u28))))
        (match (get highest-bidder auction)
            bidder (try! (as-contract (stx-transfer? (get highest-bid auction) (as-contract tx-sender) bidder)))
            true
        )
        (ok true)
    )
)
