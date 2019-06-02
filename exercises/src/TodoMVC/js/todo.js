const Core = require("./Core")
const app = require("express")()

const createStore = currentState => {
  return cb => {
    return (req, res, next) => {
      const store = {
        state: currentState,
        update: newState => (currentState = newState)
      }

      cb(store, req, res)
    }
  }
}

const withState = createStore(Core.initialState)

const decodeInputOrFail = (state, req, cb) => {
  // something something req.body
}

const getAll = withState(({ state, update }, req, res) => {
  res.json(Core.stateToList(state))
})

const postTodo = withState(({ state, update }, req, res) => {
  decodeInputOrFail(state, req, input => {
    const [newState, newTodo] = Core.createTodo(state, input)
    update(newState)
    res.json(newTodo)
  })
})

app.get("/api", getAll)
app.post("/api", postTodo)

app.listen(7879)
