function getCookie(){
  let res = document.cookie;
  Shiny.setInputValue('cookie', res);
}

function setCookie(cname,cvalue) {
  const d = new Date();
  d.setTime(d.getTime() + (1*24*60*60*1000));
  let expires = "expires=" + d.toUTCString();
  document.cookie = cname + "=" + cvalue + ";" + expires + ";path=/";
}

/*Shiny.addCustomMessageHandler('cookie-set', function(msg) {
  Cookies.set(msg.name, msg.value);
  getCookies();
})

Shiny.addCustomMessageHandler('cookie-remove', function(msg) {
  Cookies.remove(msg.name);
  getCookies();
})*/

$(document).on('shiny:connected', function(ev){
  getCookie();
})