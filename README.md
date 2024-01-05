A working Purs-nix Purescript Halogen app using Vite for bundling and serving.  

To get this up and running using nix, I like to load it in direnv but you could use 

```
git clone https://github.com/harryprayiv/AutoDraft_PS.git
cd AutoDraft_PS/
nix develop
purs-nix compile
vite --open
```

## AutoDraft_PS: Front End for my Fantasy Baseball Dapp
AutoDraft_PS is an exploratory project employing PureScript and the Halogen framework that is part of a much larger project where I create a decentralized application for fantasy baseball. This project here serves as a practical exercise in helping me learn PureScript by creating a dapp where I can focus on functional programming paradigms to manage state and side effects within an interactive user interface.

# Functional Overview
The application's core functionality is to enable users to load and interact with a list of players, each associated with projected statistics. This data forms the foundation upon which users can apply various transformations, including sorting, filtering, and reordering, to align the rankings with their strategic preferences.

# Drafting Process
Upon finalizing their preferences, users submit their rankings to the automated drafting system. The system autonomously drafts players based on the provided criteria. Post-draft, users retain the ability to fine-tune their rosters, making specific alterations based on the pool of undrafted players. This phase adheres to a strict alternating order to maintain fairness and strategic depth.

# Conflict Resolution and Contract Finalization
In scenarios where both teams vie for the same player, a rotating seniority-based resolution mechanism is employed, ensuring a deterministic and equitable outcome. Upon the establishment of final rosters, the application engages a smart contract, securing the terms until the conclusion of the designated gaming period. Notably, rescheduled, cancelled games, and games that don't reach a conclusion in the contract's strict time-frame are excluded from consideration to maintain strict determinism.

# Failsafe Mechanisms Using Incentivization
AutoDraft_PS incorporates a series of failsafe mechanisms to address potential contingencies. Should neither team submit a valid roster within the stipulated timeframe, the contract autonomously initiates a refund (minus any gas/fees), thereby annulling the draft. Conversely, if only one team fails in their submission, the compliant party is refunded their share AND the failing team's refund (minus any gas/fees), thereby incentivizing diligence and strategic engagement.

# Goals
In its essence, AutoDraft_PS is an exploration of the confluence between functional programming and decentralized technologies. It aims to demonstrate the utility and elegance of PureScript and Halogen in creating robust, user-centric applications within the burgeoning domain of DApps. As we iterate and refine its capabilities, I anticipate unveiling a platform that is not only technically sound but also strategically engaging, decentralized, and fair.

# Current Concerns:
- manually reordering the data