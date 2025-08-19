# Linguadao

A decentralized autonomous organization (DAO) designed to preserve endangered languages through community-driven initiatives and blockchain-based incentives.

## Overview

Linguadao enables communities to propose, fund, and contribute to endangered language preservation projects. Contributors earn rewards for verified contributions, while the DAO governance system ensures transparent decision-making through reputation-weighted voting.

## Core Features

- **Language Registration**: Propose new endangered languages for preservation
- **Contributor System**: Register as a contributor and build reputation
- **Reward Mechanism**: Earn STX tokens for verified contributions
- **Milestone System**: Complete language preservation milestones for additional rewards
- **DAO Governance**: Create and vote on proposals using reputation-weighted voting
- **Treasury Management**: Community-funded treasury for sustainable operations

## Smart Contract Functions

### Public Functions

#### Language Management
- `register-language(name, description)` - Propose a new language (requires 1 STX stake)
- `contribute-to-language(language-id, contribution-type, amount)` - Submit contribution
- `verify-contribution(language-id, contributor)` - Verify and reward contributions

#### Contributor System
- `register-contributor()` - Join as a contributor
- `complete-milestone(language-id, milestone-id)` - Complete language milestones

#### Governance
- `create-proposal(title, description, type, target-language, amount)` - Create DAO proposal
- `vote-on-proposal(proposal-id, vote-for)` - Vote with reputation weight
- `execute-proposal(proposal-id)` - Execute approved proposals

#### Treasury
- `fund-treasury(amount)` - Add funds to the treasury
- `add-milestone(language-id, title, description, reward-amount)` - Create milestones

### Read-Only Functions
- `get-language(language-id)` - Get language details
- `get-contributor(contributor)` - Get contributor info
- `get-proposal(proposal-id)` - Get proposal details
- `get-contribution(language-id, contributor)` - Get contribution details
- `get-milestone(language-id, milestone-id)` - Get milestone info
- `get-treasury-balance()` - Check treasury balance
- `get-total-languages()` - Total registered languages
- `get-total-contributors()` - Total contributors

## Usage Instructions

### 1. Register as Contributor
```clarity
(contract-call? .linguadao register-contributor)
```

### 2. Register a Language
```clarity
(contract-call? .linguadao register-language "Quechua" "Ancient Incan language")
```

### 3. Contribute to Language
```clarity
(contract-call? .linguadao contribute-to-language u1 "translation" u100)
```

### 4. Create Proposal
```clarity
(contract-call? .linguadao create-proposal 
  "Fund Quechua Dictionary" 
  "Community dictionary project" 
  "funding" 
  u1 
  u500000)
```

### 5. Vote on Proposal
```clarity
(contract-call? .linguadao vote-on-proposal u1 true)
```

## Constants

- **MIN-PROPOSAL-THRESHOLD**: 1,000,000 µSTX (1 STX)
- **VOTING-PERIOD**: 1,440 blocks (~10 days)
- **REWARD-MULTIPLIER**: 150% of contribution amount

## Error Codes

- `u401` - Not authorized
- `u402` - Already exists
- `u403` - Insufficient funds
- `u404` - Not found
- `u405` - Voting closed
- `u406` - Already voted
- `u407` - Invalid amount
- `u408` - Proposal active

## Deployment

1. Deploy contract using Clarinet
2. Fund initial treasury
3. Register first contributors
4. Begin language preservation initiatives

## License

MIT License
