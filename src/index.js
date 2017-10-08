import './main.css';
import { Main } from './Main.elm';

var app = Main.embed(document.getElementById('root'));

app.ports.requestOffsets.subscribe(function() {
  sendOffsetsWhenBoardAvailable();
});

function sendOffsetsWhenBoardAvailable() {
  var board = document.getElementById("board");

  if (board === null) {
    setTimeout(sendOffsetsWhenBoardAvailable, 1000);
  } else {
    var cell = board.getElementsByClassName("cell")[0];

    if (cell === null) {
      setTimeout(sendOffsetsWhenBoardAvailable, 1000);
    } else {
      sendOffsets(board, cell);
    }
  }
}

function sendOffsets(board, cell) {
  console.log("Sending offsets!");

  var boardRect = board.getBoundingClientRect(),
      cellRect  = cell.getBoundingClientRect();

  app.ports.receiveOffsets.send(
    [ {x: Math.round(boardRect.x),    y: Math.round(boardRect.y)}
    , {x: Math.round(cellRect.width), y: Math.round(cellRect.height) }
    ]);
}
