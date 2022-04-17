import Alpine from 'alpinejs'
window.Alpine = Alpine
Alpine.start()

const checkbox = document.querySelector("#darkmode-toggle");
const html = document.querySelector("html");

const toggleDarkMode = () => {
  if (checkbox.checked) {
    html.classList.add("dark");
    localStorage.theme = "dark";
    console.log("[+] Enabling and saving dark mode preference");
  } else {
    html.classList.remove("dark");
    localStorage.theme = "light";
    console.log("[+] Enabling and saving light mode preference");
  }
}

checkbox.addEventListener("click", toggleDarkMode);
