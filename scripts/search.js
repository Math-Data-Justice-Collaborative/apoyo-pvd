document.addEventListener('DOMContentLoaded', function() {
    const searchInput = document.getElementById('search-input');
    const searchBar = document.getElementById('search-bar');
    let searchResults = [];

    // Create search results container
    const resultsContainer = document.createElement('div');
    resultsContainer.className = 'search-results';
    searchBar.appendChild(resultsContainer);

    // Function to filter companies based on search input
    function filterCompanies(searchTerm) {
        // Get all chapter containers
        const chapters = document.querySelectorAll('.chapter-container');
        searchResults = [];

        chapters.forEach(chapter => {
            const title = chapter.querySelector('.chapter-header')?.textContent.toLowerCase() || '';
            const address = chapter.querySelector('.chapter-address')?.textContent.toLowerCase() || '';
            const phone = chapter.querySelector('.chapter-phone')?.textContent.toLowerCase() || '';

            if (title.includes(searchTerm) || 
                address.includes(searchTerm) || 
                phone.includes(searchTerm)) {
                searchResults.push({
                    element: chapter,
                    title: title,
                    address: address,
                    phone: phone
                });
            }
        });

        displayResults();
    }

    // Function to display search results
    function displayResults() {
        resultsContainer.innerHTML = '';
        
        if (searchResults.length === 0) {
            resultsContainer.style.display = 'none';
            return;
        }

        searchResults.forEach(result => {
            const resultItem = document.createElement('div');
            resultItem.className = 'search-result-item';
            resultItem.innerHTML = `
                <div><strong>${result.title}</strong></div>
                <div>${result.address}</div>
                <div>${result.phone}</div>
            `;

            resultItem.addEventListener('click', () => {
                // Scroll to the result
                result.element.scrollIntoView({ behavior: 'smooth' });
                // Highlight the result
                result.element.style.backgroundColor = '#fff3cd';
                setTimeout(() => {
                    result.element.style.backgroundColor = '';
                }, 2000);
                // Clear search
                searchInput.value = '';
                resultsContainer.style.display = 'none';
            });

            resultsContainer.appendChild(resultItem);
        });

        resultsContainer.style.display = 'block';
    }

    // Add event listeners
    searchInput.addEventListener('input', (e) => {
        const searchTerm = e.target.value.toLowerCase();
        if (searchTerm.length > 0) {
            filterCompanies(searchTerm);
        } else {
            resultsContainer.style.display = 'none';
        }
    });

    // Close search results when clicking outside
    document.addEventListener('click', (e) => {
        if (!searchBar.contains(e.target)) {
            resultsContainer.style.display = 'none';
        }
    });
}); 