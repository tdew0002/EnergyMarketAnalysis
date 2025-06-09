// highlight the current section in the sidebar TOC
document.addEventListener("scroll", () => {
  const headings = document.querySelectorAll("h1, h2, h3");
  let idx = headings.length;
  while (idx-- && window.scrollY + 85 < headings[idx].offsetTop) {}
  document.querySelectorAll("#TOC li").forEach(li => li.classList.remove("toc-active"));
  if (idx >= 0) {
    const id = headings[idx].id;
    const active = document.querySelector(`#TOC a[href="#${id}"]`)?.parentElement;
    if (active) active.classList.add("toc-active");
  }
});
