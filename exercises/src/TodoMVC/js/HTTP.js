const express = require("express")
const cors = require("cors")

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

const httpHandler = method => (path, handler) => (app, store) => {
  app[method](path, (rawReq, res, next) => {
    const req = {} // todo
    const state = store.state
    handler(state, req, ([newState, response]) => {
      store.update(newState)
      if (response.type === "json") {
        res.json(response.body)
      }
      res.json("WTF")
    })
  })
}

const startServer = (initialState, handlers) => {
  const app = express()
  const store = createStore(initialState)

  app.use(express.json())
  app.use(cors())
  handlers.forEach(h => h(app, store))

  console.log("Starting server...")
  app.listen(7879)
}

module.exports = {
  get: httpHandler,
  post: httpHandler,
  patch: httpHandler,
  delete: httpHandler,
  startServer
}
