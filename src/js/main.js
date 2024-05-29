import '../css/styles.css'
import { Elm } from '../elm/Main.elm'

var storedData = localStorage.getItem('data');
var flags = storedData ? JSON.parse(storedData) : null;

// Start the Elm application.
const app = Elm.Main.init({
  node: document.querySelector('main'),
  flags: flags 
})

app.ports.setStorage.subscribe(function (data){
  localStorage.setItem("data", JSON.stringify(data))
})
