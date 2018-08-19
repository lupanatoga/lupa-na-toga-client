var table = document.getElementsByClassName('shiny-table');
var arr = [].slice.call(table);
arr.forEach(tab => tab.classList.add('table-striped'))