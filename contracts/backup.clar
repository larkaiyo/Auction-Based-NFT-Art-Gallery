(define-non-fungible-token art-nft uint)

(define-constant contract-owner tx-sender)
(define-constant min-auction-duration u100)
(define-constant max-auction-duration u10000)
(define-constant royalty-percentage u5)
(define-constant anti-snipe-blocks u12)
(define-constant buy-now-multiplier u150)
(define-constant min-buy-now-multiplier u120)

;; Featured Artists Error Constants
(define-constant err-not-gallery-owner u100)
(define-constant err-artist-already-featured u101)
(define-constant err-artist-not-featured u102)
(define-constant err-max-featured-reached u103)
(define-constant err-invalid-position u104)
(define-constant err-invalid-reputation-score u105)
(define-constant err-insufficient-stats u106)
(define-constant err-invalid-rating u107)

(define-data-var nft-counter uint u0)
(define-data-var gallery-owner principal tx-sender)
(define-data-var featured-artists-count uint u0)
(define-data-var max-featured-artists uint u10)

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

(define-map buy-now-prices
    uint
    uint
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

(define-map auction-history
    uint
    {
        creator: principal,
        winner: (optional principal),
        final-price: uint,
        total-extensions: uint,
        completion-block: uint
    }
)

(define-map user-stats
    principal
    {
        auctions-created: uint,
        auctions-won: uint,
        total-spent: uint,
        total-earned: uint
    }
)

;; Featured Artists Showcase System
(define-map featured-artists
    principal
    {
        featured-since: uint,
        reputation-score: uint,
        position: uint,
        showcase-description: (string-ascii 200),
        total-featured-sales: uint
    }
)

(define-map artist-reputation
    principal
    {
        success-rate: uint,
        avg-sale-price: uint,
        community-rating: uint,
        total-volume: uint,
        featured-count: uint
    }
)

(define-map gallery-curation
    { curator: principal, position: uint }
    {
        artist: principal,
        curated-at: uint,
        reason: (string-ascii 150)
    }
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

(define-read-only (get-auction-history (auction-id uint))
    (map-get? auction-history auction-id)
)

(define-read-only (get-user-stats (user principal))
    (default-to 
        { auctions-created: u0, auctions-won: u0, total-spent: u0, total-earned: u0 }
        (map-get? user-stats user)
    )
)

(define-read-only (get-top-creators (limit uint))
    (ok limit)
)

(define-read-only (get-top-bidders (limit uint))
    (ok limit)
)

(define-read-only (get-buy-now-price (auction-id uint))
    (map-get? buy-now-prices auction-id)
)

(define-read-only (calculate-dynamic-buy-now-price (auction-id uint))
    (match (map-get? auctions auction-id)
        auction (let (
            (current-highest-bid (get highest-bid auction))
            (base-price (get reserve-price auction))
        )
            (if (> current-highest-bid u0)
                (/ (* current-highest-bid buy-now-multiplier) u100)
                (/ (* base-price buy-now-multiplier) u100)
            )
        )
        u0
    )
)

;; Featured Artists Showcase Read-Only Functions
(define-read-only (get-featured-artist (artist principal))
    (map-get? featured-artists artist)
)

(define-read-only (get-artist-reputation (artist principal))
    (default-to 
        { success-rate: u0, avg-sale-price: u0, community-rating: u50, total-volume: u0, featured-count: u0 }
        (map-get? artist-reputation artist)
    )
)

(define-read-only (is-artist-featured (artist principal))
    (is-some (map-get? featured-artists artist))
)

(define-read-only (get-featured-artists-count)
    (var-get featured-artists-count)
)

(define-read-only (get-gallery-owner)
    (var-get gallery-owner)
)

(define-read-only (calculate-reputation-score (artist principal))
    (let (
        (stats (get-user-stats artist))
        (auctions-created (get auctions-created stats))
        (total-earned (get total-earned stats))
    )
        (if (> auctions-created u0)
            (let (
                (success-rate (/ (* (get auctions-won stats) u100) auctions-created))
                (avg-earnings (if (> auctions-created u0) (/ total-earned auctions-created) u0))
                (volume-score-calc (/ total-earned u1000000))
                (volume-score (if (< volume-score-calc u50) volume-score-calc u50))
                (earnings-score-calc (/ avg-earnings u100000))
                (earnings-score (if (< earnings-score-calc u30) earnings-score-calc u30))
            )
                (+ success-rate volume-score earnings-score)
            )
            u0
        )
    )
)

(define-read-only (get-curation-info (curator principal) (position uint))
    (map-get? gallery-curation { curator: curator, position: position })
)

(define-read-only (is-buy-now-available (auction-id uint))
    (match (map-get? auctions auction-id)
        auction (and 
            (is-eq (get status auction) "active")
            (< stacks-block-height (get end-block auction))
            (is-some (map-get? buy-now-prices auction-id))
        )
        false
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
        (update-user-stats-on-auction-start tx-sender)
        (ok true)
    )
)

(define-public (set-buy-now-price (auction-id uint) (buy-now-price uint))
    (let ((auction (unwrap! (map-get? auctions auction-id) (err u50))))
        (asserts! (is-eq tx-sender (get creator auction)) (err u51))
        (asserts! (is-eq (get status auction) "active") (err u52))
        (asserts! (>= buy-now-price (/ (* (get reserve-price auction) min-buy-now-multiplier) u100)) (err u53))
        (map-set buy-now-prices auction-id buy-now-price)
        (ok true)
    )
)

(define-public (instant-purchase (auction-id uint))
    (let (
        (auction (unwrap! (map-get? auctions auction-id) (err u54)))
        (buy-now-price (unwrap! (map-get? buy-now-prices auction-id) (err u55)))
        (creator (get creator auction))
        (highest-bidder (get highest-bidder auction))
        (highest-bid (get highest-bid auction))
        (royalty (/ (* buy-now-price royalty-percentage) u100))
        (creator-payment (- buy-now-price royalty))
    )
        (asserts! (is-eq (get status auction) "active") (err u56))
        (asserts! (< stacks-block-height (get end-block auction)) (err u57))
        
        (try! (stx-transfer? buy-now-price tx-sender (as-contract tx-sender)))
        
        (match highest-bidder
            bidder (try! (as-contract (stx-transfer? highest-bid (as-contract tx-sender) bidder)))
            true
        )
        
        (try! (as-contract (nft-transfer? art-nft auction-id tx-sender tx-sender)))
        (try! (as-contract (stx-transfer? creator-payment (as-contract tx-sender) creator)))
        (try! (as-contract (stx-transfer? royalty (as-contract tx-sender) creator)))
        
        (map-set artist-royalties creator (+ (get-artist-royalties creator) royalty))
        (map-set auctions auction-id (merge auction { status: "sold" }))
        
        (record-auction-completion auction-id auction (some tx-sender) buy-now-price)
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
        (record-auction-completion auction-id auction highest-bidder highest-bid)
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

(define-private (update-user-stats-on-auction-start (creator principal))
    (let ((current-stats (get-user-stats creator)))
        (map-set user-stats creator (merge current-stats {
            auctions-created: (+ (get auctions-created current-stats) u1)
        }))
    )
)

(define-private (record-auction-completion (auction-id uint) (auction { creator: principal, reserve-price: uint, end-block: uint, highest-bid: uint, highest-bidder: (optional principal), status: (string-ascii 20), extensions: uint }) (winner (optional principal)) (final-price uint))
    (begin
        (map-set auction-history auction-id {
            creator: (get creator auction),
            winner: winner,
            final-price: final-price,
            total-extensions: (get extensions auction),
            completion-block: stacks-block-height
        })
        
        (let ((creator-stats (get-user-stats (get creator auction))))
            (map-set user-stats (get creator auction) (merge creator-stats {
                total-earned: (+ (get total-earned creator-stats) final-price)
            }))
        )
        
        (match winner
            bidder (let ((bidder-stats (get-user-stats bidder)))
                (map-set user-stats bidder (merge bidder-stats {
                    auctions-won: (+ (get auctions-won bidder-stats) u1),
                    total-spent: (+ (get total-spent bidder-stats) final-price)
                }))
            )
            true
        )
        
        ;; Update reputation for featured artists
        (if (is-artist-featured (get creator auction))
            (match (map-get? featured-artists (get creator auction))
                featured-artist (map-set featured-artists (get creator auction) 
                    (merge featured-artist { total-featured-sales: (+ (get total-featured-sales featured-artist) u1) }))
                true
            )
            true
        )
        
        ;; Update reputation stats
        (let (
            (creator (get creator auction))
            (current-rep (get-artist-reputation creator))
            (stats (get-user-stats creator))
        )
            (map-set artist-reputation creator {
                success-rate: (if (> (get auctions-created stats) u0) 
                               (/ (* (get auctions-won stats) u100) (get auctions-created stats)) 
                               u0),
                avg-sale-price: (if (> (get auctions-created stats) u0) 
                                  (/ (get total-earned stats) (get auctions-created stats)) 
                                  u0),
                community-rating: (get community-rating current-rep),
                total-volume: (get total-earned stats),
                featured-count: (get featured-count current-rep)
            })
        )
    )
)

;; Featured Artists Showcase Public Functions

(define-public (feature-artist (artist principal) (position uint) (showcase-description (string-ascii 200)))
    (let (
        (featured-count (var-get featured-artists-count))
        (reputation-score (calculate-reputation-score artist))
    )
        (asserts! (is-eq tx-sender (var-get gallery-owner)) (err err-not-gallery-owner))
        (asserts! (not (is-some (map-get? featured-artists artist))) (err err-artist-already-featured))
        (asserts! (< featured-count (var-get max-featured-artists)) (err err-max-featured-reached))
        (asserts! (and (>= position u1) (<= position (+ featured-count u1))) (err err-invalid-position))
        (asserts! (>= reputation-score u20) (err err-invalid-reputation-score))
        
        ;; Verify artist has completed at least one auction
        (asserts! (> (get auctions-created (get-user-stats artist)) u0) (err err-insufficient-stats))
        
        ;; Add artist to featured showcase
        (map-set featured-artists artist {
            featured-since: stacks-block-height,
            reputation-score: reputation-score,
            position: position,
            showcase-description: showcase-description,
            total-featured-sales: u0
        })
        
        ;; Update curator information
        (map-set gallery-curation { curator: tx-sender, position: position } {
            artist: artist,
            curated-at: stacks-block-height,
            reason: showcase-description
        })
        
        ;; Update artist reputation
        (let ((current-rep (get-artist-reputation artist)))
            (map-set artist-reputation artist (merge current-rep {
                featured-count: (+ (get featured-count current-rep) u1)
            }))
        )
        
        ;; Increment featured count
        (var-set featured-artists-count (+ featured-count u1))
        (ok true)
    )
)

(define-public (remove-featured-artist (artist principal))
    (let ((featured-info (unwrap! (map-get? featured-artists artist) (err err-artist-not-featured))))
        (asserts! (is-eq tx-sender (var-get gallery-owner)) (err err-not-gallery-owner))
        
        ;; Remove artist from featured showcase
        (map-delete featured-artists artist)
        
        ;; Update count
        (var-set featured-artists-count (- (var-get featured-artists-count) u1))
        
        ;; Remove curation info
        (map-delete gallery-curation { curator: tx-sender, position: (get position featured-info) })
        
        (ok true)
    )
)

(define-public (update-artist-position (artist principal) (new-position uint))
    (let ((featured-info (unwrap! (map-get? featured-artists artist) (err err-artist-not-featured))))
        (asserts! (is-eq tx-sender (var-get gallery-owner)) (err err-not-gallery-owner))
        (asserts! (and (>= new-position u1) (<= new-position (var-get featured-artists-count))) (err err-invalid-position))
        
        ;; Update artist position
        (map-set featured-artists artist (merge featured-info {
            position: new-position
        }))
        
        ;; Update curation info
        (map-delete gallery-curation { curator: tx-sender, position: (get position featured-info) })
        (map-set gallery-curation { curator: tx-sender, position: new-position } {
            artist: artist,
            curated-at: stacks-block-height,
            reason: (get showcase-description featured-info)
        })
        
        (ok true)
    )
)

(define-public (update-showcase-description (artist principal) (new-description (string-ascii 200)))
    (let ((featured-info (unwrap! (map-get? featured-artists artist) (err err-artist-not-featured))))
        (asserts! (or (is-eq tx-sender (var-get gallery-owner)) (is-eq tx-sender artist)) (err err-not-gallery-owner))
        
        ;; Update description
        (map-set featured-artists artist (merge featured-info {
            showcase-description: new-description
        }))
        
        ;; Update curation info if gallery owner
        (if (is-eq tx-sender (var-get gallery-owner))
            (map-set gallery-curation { curator: tx-sender, position: (get position featured-info) } {
                artist: artist,
                curated-at: stacks-block-height,
                reason: new-description
            })
            true
        )
        
        (ok true)
    )
)

(define-public (rate-artist (artist principal) (rating uint))
    (asserts! (and (>= rating u0) (<= rating u100)) (err err-invalid-rating))
    
    ;; Update artist reputation with new community rating
    (let ((current-rep (get-artist-reputation artist)))
        (map-set artist-reputation artist (merge current-rep {
            community-rating: rating
        }))
        (ok true)
    )
)

(define-public (update-max-featured-artists (new-max uint))
    (begin
        (asserts! (is-eq tx-sender (var-get gallery-owner)) (err err-not-gallery-owner))
        (asserts! (>= new-max (var-get featured-artists-count)) (err err-max-featured-reached))
        (var-set max-featured-artists new-max)
        (ok true)
    )
)

(define-public (transfer-gallery-ownership (new-owner principal))
    (begin
        (asserts! (is-eq tx-sender (var-get gallery-owner)) (err err-not-gallery-owner))
        (var-set gallery-owner new-owner)
        (ok true)
    )
)
