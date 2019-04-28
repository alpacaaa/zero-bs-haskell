
const app = require("express")()
const bodyParser = require("body-parser")

app.use(bodyParser.json())

// Mutable state!
// Items are indexed by "model"
let currentCart = {}

app.post("/cart", (req, res) => {
  const item = req.body

  if (item.model && item.quantity)
  {
    const existingQuantity = currentCart[item.model] ? currentCart[item.model].quantity : 0
    item.quantity = existingQuantity + item.quantity
    currentCart[item.model] = item
    res.send("ok")
  }
  else {
    throw new Error("Not a valid item")
  }
})

app.get("/cart", (req, res) => {
  const items = Object.values(currentCart)
  // Items should be sorted by quantity
  res.json(items.sort((a, b) => a.quantity < b.quantity))
})

console.log("Starting server...")
app.listen(7879)
