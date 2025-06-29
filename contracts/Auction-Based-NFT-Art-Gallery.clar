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
        status: (string-ascii 20)
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

(define-read-only (get-nft-owner (token-id uint))
    (nft-get-owner? art-nft token-id)
)

(define-read-only (get-auction (auction-id uint))
    (map-get? auctions auction-id)
)

(define-read-only (get-bid (auction-id uint) (bidder principal))
    (map-get? bids { auction-id: auction-id, bidder: bidder })
)

(define-read-only (get-artist-royalties (artist principal))
    (default-to u0 (map-get? artist-royalties artist))
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
        (try! (nft-transfer? art-nft token-id tx-sender (as-contract tx-sender)))
        (map-set auctions token-id {
            creator: tx-sender,
            reserve-price: reserve-price,
            end-block: (+ stacks-block-height duration),
            highest-bid: u0,
            highest-bidder: none,
            status: "active"
        })
        (ok true)
    )
)

(define-public (place-bid (auction-id uint) (bid-amount uint))
    (let (
        (auction (unwrap! (map-get? auctions auction-id) (err u5)))
        (current-bid (default-to u0 (map-get? bids { auction-id: auction-id, bidder: tx-sender })))
        (total-bid (+ current-bid bid-amount))
    )
        (asserts! (is-eq (get status auction) "active") (err u6))
        (asserts! (> stacks-block-height (get end-block auction)) (err u7))
        (asserts! (> total-bid (get highest-bid auction)) (err u8))
        (try! (stx-transfer? bid-amount tx-sender (as-contract tx-sender)))
        (map-set bids { auction-id: auction-id, bidder: tx-sender } total-bid)
        (map-set auctions auction-id (merge auction {
            highest-bid: total-bid,
            highest-bidder: (some tx-sender)
        }))
        (ok true)
    )
)

(define-public (end-auction (auction-id uint))
    (let (
        (auction (unwrap! (map-get? auctions auction-id) (err u9)))
        (highest-bidder (get highest-bidder auction))
        (highest-bid (get highest-bid auction))
        (creator (get creator auction))
    )
        (asserts! (>= stacks-block-height (get end-block auction)) (err u10))
        (asserts! (is-eq (get status auction) "active") (err u11))
        
        (if (and (is-some highest-bidder) (>= highest-bid (get reserve-price auction)))
            (let (
                (royalty (/ (* highest-bid royalty-percentage) u100))
                (creator-payment (- highest-bid royalty))
            )
                (try! (as-contract (nft-transfer? art-nft auction-id tx-sender (unwrap! highest-bidder (err u12)))))
                (try! (as-contract (stx-transfer? creator-payment (as-contract tx-sender) creator)))
                (try! (as-contract (stx-transfer? royalty (as-contract tx-sender) creator)))
                (map-set artist-royalties creator (+ (get-artist-royalties creator) royalty))
            )
            (begin
                (try! (as-contract (nft-transfer? art-nft auction-id tx-sender creator)))
                (try! (refund-bids auction-id))
            )
        )
        
        (map-set auctions auction-id (merge auction { status: "ended" }))
        (ok true)
    )
)

(define-private (refund-bids (auction-id uint))
    (let ((auction (unwrap! (map-get? auctions auction-id) (err u13))))
        (match (get highest-bidder auction)
            bidder (try! (as-contract (stx-transfer? (get highest-bid auction) (as-contract tx-sender) bidder)))
            true
        )
        (ok true)
    )
)
