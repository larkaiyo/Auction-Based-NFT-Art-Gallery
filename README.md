# 🎨 Auction-Based NFT Art Gallery

A decentralized NFT art marketplace built on Stacks blockchain that enables artists to showcase and auction their digital artwork.

## 🌟 Features

- NFT minting for digital artwork
- Timed auctions with reserve prices
- Secure bidding system
- Automatic royalty distribution
- Anti-sniping protection
- Transparent auction history

## 🚀 Smart Contract Functions

### For Artists
- `mint-nft`: Create a new NFT for your artwork
- `start-auction`: Start an auction for your NFT with custom duration and reserve price

### For Collectors
- `place-bid`: Place a bid on active auctions
- `get-auction`: View auction details
- `get-bid`: Check your current bid

### System Functions
- `end-auction`: Automatically finalizes auction and handles transfers
- `get-artist-royalties`: View accumulated artist royalties

## 💎 Usage

1. Connect your Stacks wallet
2. Artists: Mint your NFT using `mint-nft`
3. Start auction with `start-auction`
4. Collectors can place bids using `place-bid`
5. Auction ends automatically after duration
6. Winners receive NFT, artists receive payment + royalties

## ⚡ Technical Details

- Minimum auction duration: 100 blocks
- Maximum auction duration: 10000 blocks
- Royalty percentage: 5%
- Anti-snipe protection: 12 blocks
```
