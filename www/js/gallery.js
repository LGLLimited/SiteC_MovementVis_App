let currentSlideIndex = 0;
const images = document.querySelectorAll('.slider img');
const dots = document.querySelectorAll('.dot');

// Function to change the slide based on the dot clicked
function changeSlide(index) {
  // Remove 'active' class from all images
  images.forEach(img => {
    img.classList.remove('active');
  });

  // Add 'active' class to the image that corresponds to the clicked dot
  images[index].classList.add('active');

  // Remove 'active' class from all dots
  dots.forEach(dot => {
    dot.classList.remove('active');
  });

  // Add 'active' class to the clicked dot
  dots[index].classList.add('active');

  // Update the current slide index
  currentSlideIndex = index;
}

// Optional: Auto slide change every 3 seconds
setInterval(() => {
  currentSlideIndex = (currentSlideIndex + 1) % images.length;
  changeSlide(currentSlideIndex);
}, 9000);

// Initial setup to display the first image correctly
changeSlide(0);
