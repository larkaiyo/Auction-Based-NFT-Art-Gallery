Featured Artists Showcase System

Overview
Enhanced the NFT Art Gallery with a comprehensive Featured Artists Showcase system that allows gallery owners to highlight top-performing artists, manage artist reputations, and provide community-driven ratings. This independent feature adds significant value by creating an artist discovery mechanism and reputation-based curation system.

Technical Implementation
- **Featured Artists Data Management**: New maps for `featured-artists`, `artist-reputation`, and `gallery-curation` with comprehensive tracking
- **Reputation Scoring**: Dynamic reputation calculation based on success rate, average sale price, total volume, and community ratings  
- **Gallery Curation Functions**: Complete CRUD operations for featured artist management including positioning and showcase descriptions
- **Community Rating System**: User-driven artist rating functionality with 0-100 scale
- **Owner Controls**: Gallery ownership transfer and maximum featured artists configuration

Key Functions Added:
- `feature-artist`: Add artists to featured showcase with position and description
- `remove-featured-artist`: Remove artists from featured showcase
- `update-artist-position`: Manage featured artist positioning 
- `update-showcase-description`: Update artist showcase descriptions
- `rate-artist`: Community rating system for artists
- `calculate-reputation-score`: Dynamic reputation scoring algorithm
- `get-featured-artist`: Retrieve featured artist information
- `get-artist-reputation`: Get comprehensive reputation data

Testing & Validation
- ✅ Contract passes clarinet check with zero errors
- ✅ All npm tests successful (1 passed)
- ✅ CI/CD pipeline configured with GitHub Actions
- ✅ Clarity v3 compliant with proper error handling (7 new error constants)
- ✅ Independent implementation with no cross-contract dependencies
- ✅ Line endings normalized to LF format
