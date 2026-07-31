$( document ).ready(function() {
    $("header").find("nav").append('<span>DetectoR: Interactive Safety Data Insights Tool</span>');
})

// To adapt for mod_table
// # Bold cells for those >= 15 in the first column
// rowCallback = DT::JS(
//   "function(row, data) {\n
//             if (parseFloat(data[1]) >= 15.0)\n
//             $(\"td:eq(1)\", row).css(\"font-weight\", \"bold\");\n
//             }"
// )
