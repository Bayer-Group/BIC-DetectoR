$( document ).ready(function() {
    $("header").find("nav").append('<span>DetectoR: Interactive Safety Data Insights Tool</span>');

    // Append the toggle theme button
    const toggle = $("#toggle-theme-container");
    $("header").find("nav").append(toggle)
})

// To adapt for mod_table
// # Bold cells for those >= 15 in the first column
// rowCallback = DT::JS(
//   "function(row, data) {\n
//             if (parseFloat(data[1]) >= 15.0)\n
//             $(\"td:eq(1)\", row).css(\"font-weight\", \"bold\");\n
//             }"
// )

// Toggle light-dark mode

// Source - https://stackoverflow.com/a/26107174
// Posted by Rob M., modified by community. See post 'Timeline' for change history
// Retrieved 2026-02-27, License - CC BY-SA 4.0

window.addEventListener("DOMContentLoaded", (event) => {
    const el = document.querySelector('#toggle_theme');
    el?.addEventListener('click', () => {
        document.body.classList.toggle('dark');
    });
});
