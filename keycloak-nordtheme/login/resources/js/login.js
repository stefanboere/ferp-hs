window.onload = function() {

  var currentLocale = document.getElementById('kc-current-locale-link');

  if(currentLocale == null)
    return;

  currentLocale.onclick = function(event){
    var dropdown = document.getElementById('kc-locale-dropdown');

    if(dropdown.classList.contains('open'))
      dropdown.classList.remove('open');
    else
      dropdown.classList.add('open');
  }
}

window.onclick = function(event) {
  if (!event.target.matches('#kc-current-locale-link')) {
    var dropdown = document.getElementById('kc-locale-dropdown');
    if(dropdown.classList.contains('open'))
      dropdown.classList.remove('open');
  }
}
