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

// ------
const getAll = (state, _, next) => {
  next(state, jsonResponse(Core.stateToList(state)))
}

const postTodo = (state, req, next) => {
  decodeInputOrFail(state, req, input => {
    if (input.title === null) {
      next(state, failureResponse("Empty title"))
    } else {
      const [newState, newTodo] = Core.createTodo(state, input)
      next(newState, jsonResponse(newTodo))
    }
  })
}

const findTodoOrFail = (state, req, cb) => {
  let urlId = req.urlParam("id")

  if (urlId === null) {
    return [state, failureResponse("No :id found in URL")]
  } else {
    let todo = Core.findTodo(state, urlId)

    if (todo === null) {
      return [state, failureResponse("Todo not found")]
    } else {
      return cb(todo)
    }
  }
}

const patchTodo = (state, req, next) => {
  findTodoOrFail(state, req, existing => {
    decodeInputOrFail(state, req, input => {
      let [newState, updated] = Core.updateTodo(
        state,
        existing,
        input.title,
        input.completed,
        input.order
      )
      next(newState, jsonResponse(updated))
    })
  })
}

HTTP.startServer(Core.initialState, [
  HTTP.get("/api", getAll),
  HTTP.post("/api", postTodo),
  HTTP.delete("/api", deleteAll),
  HTTP.get("/api/:id", getTodo),
  HTTP.patch("/api/:id", patchTodo),
  HTTP.delete("/api/:id", deleteTodo)
])
