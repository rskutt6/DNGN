#lang brag
program : room*
room : /LPAREN /"room" ID room-body /RPAREN
room-body : room-element*
          
room-element : desc
             | item
             | monster
             | exit
             | power
desc : /LPAREN /"desc" STRING /RPAREN
item : /LPAREN /"item" ID item-body /RPAREN
item-body : item-field*
          
item-field : desc
           | type
type : /LPAREN /"type" ID /RPAREN
monster : /LPAREN /"monster" ID NUMBER /RPAREN
exit : /LPAREN /"exit" ID ID key? /RPAREN
key : /LPAREN /"key" ID /RPAREN
power : /LPAREN /"power" NUMBER /RPAREN

