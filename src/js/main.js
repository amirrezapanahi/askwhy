import '../css/styles.css'
import { Elm } from '../elm/Main.elm'

const defaultData = JSON.parse(`{"threads":[{"id":1,"name":"I need to go the gym","content":[{"ponder":{"type":"Reason","reason_value":"I need to go the gym"},"why":"I need to go the gym"},{"ponder":{"type":"Reason","reason_value":"get fitter"},"why":"get fitter"},{"ponder":{"type":"Reason","reason_value":"perform better at soccer"},"why":"perform better at soccer"},{"ponder":{"type":"Principle","principle_value":"I want to feel confident in sport"},"why":"I want to feel confident in sport"}]},{"id":2,"name":"I need to study computer science","content":[{"ponder":{"type":"Reason","reason_value":"I need to study computer science"},"why":"I need to study computer science"},{"ponder":{"type":"Reason","reason_value":"I want a good job"},"why":"I want a good job"},{"ponder":{"type":"Reason","reason_value":"I want to earn a lot of money"},"why":"I want to earn a lot of money"},{"ponder":{"type":"Principle","principle_value":"I want to support my family"},"why":"I want to support my family"}]}],"currentNodeIdx":4,"currentThread":{"id":1,"name":"I need to go the gym","content":[{"ponder":{"type":"Reason","reason_value":"I need to go the gym"},"why":"I need to go the gym"},{"ponder":{"type":"Reason","reason_value":"get fitter"},"why":"get fitter"},{"ponder":{"type":"Reason","reason_value":"perform better at soccer"},"why":"perform better at soccer"},{"ponder":{"type":"Principle","principle_value":"I want to feel confident in sport"},"why":"I want to feel confident in sport"}]},"principles":[]}`)

var storedData = localStorage.getItem('data');
var flags = storedData ? JSON.parse(storedData) : defaultData;

// Start the Elm application.
const app = Elm.Main.init({
  node: document.querySelector('main'),
  flags: flags 
})

app.ports.setStorage.subscribe(function (data){
  localStorage.setItem("data", JSON.stringify(data))
})
