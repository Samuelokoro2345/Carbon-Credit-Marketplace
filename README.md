# Carbon Credit Marketplace (CCM)

A decentralized marketplace for trading carbon credits on the Stacks blockchain.

## Overview

CCM enables:
- Minting of verified carbon credits as NFTs
- Listing credits for sale
- Purchasing credits with STX
- Tracking credit metadata including amount, verification date, project details

## Contract Functions

### Minting
- `mint`: Create new carbon credits (contract owner only)

### Trading
- `list-credit`: List a credit for sale
- `unlist-credit`: Remove a credit listing
- `purchase-credit`: Buy a listed credit

### Read-Only
- `get-token-metadata`: View credit details
- `get-listing`: Check market listing
- `get-total-credits`: Get total credits issued
- `get-last-token-id`: Get latest token ID

## Usage

1. Deploy contract using Clarinet
2. Mint credits using contract owner address
3. List credits for sale with desired STX price
4. Purchase credits by sending required STX amount

## Testing

Run tests using Clarinet:
```bash
clarinet test