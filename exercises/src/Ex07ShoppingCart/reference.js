
const app = require("express")()
const bodyParser = require("body-parser")

app.use(bodyParser.json())

// Mutable state!
let currentCart = []

app.post("/cart", (req, res) => {
  const item = req.body

  if (item.model && item.quantity)
  {
    currentCart.push(item)
    res.send("ok")
  }
  else {
    throw new Error("Not a valid item")
  }
})

app.get("/cart", (req, res) => {
  res.json(currentCart)
})

console.log("Starting server...")
app.listen(7879)
