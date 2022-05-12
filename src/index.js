import { Elm } from "./Main.elm";

const app = Elm.Main.init({ node: document.getElementById("app") });

customElements.whenDefined('game-canvas').then(() => {
  app.ports.sendMessage.subscribe(function(gameData) {
    const gameCanvas = document.getElementsByTagName('game-canvas')[0]
    gameCanvas.draw(gameData)
  })

  document.body.onkeydown = function(e) {
    app.ports.bodyKeyPress.send(e.keyCode);
  };

  document.body.onkeyup = function(e) {
    app.ports.bodyKeyUp.send(e.keyCode);
  };
})