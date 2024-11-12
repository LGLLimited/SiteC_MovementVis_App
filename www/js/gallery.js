let currentSlideIndex = 0;
const images = document.querySelectorAll('.slider img');
const slider = document.querySelector('.slider');

// Function to change the slide based on the index
function changeSlide(index) {
  // Ensure the index is within bounds
  if (index < 0) {
    index = images.length - 1; // Go to the last image if index is less than 0
  } else if (index >= images.length) {
    index = 0; // Go to the first image if index exceeds the last image
  }

  // Remove 'active' class from all images
  images.forEach(img => {
    img.classList.remove('active');
  });

  // Add 'active' class to the image that corresponds to the clicked dot
  images[index].classList.add('active');

  // Update the background image of the container based on the current slide
  const currentImage = images[index];
  const currentImageSrc = currentImage.src;
  slider.style.backgroundImage = `url('${currentImageSrc}')`; // Set the background image
  slider.style.backgroundSize = 'cover'; // Ensure the background image covers the entire container
  slider.style.backgroundPosition = 'center'; // Center the background image

  // Update the current slide index
  currentSlideIndex = index;
}

// Event listeners for left and right arrows
document.getElementById('prev').addEventListener('click', () => {
  changeSlide(currentSlideIndex - 1); // Move to previous slide
});

document.getElementById('next').addEventListener('click', () => {
  changeSlide(currentSlideIndex + 1); // Move to next slide
});

// Initial setup to display the first image correctly
changeSlide(0);
