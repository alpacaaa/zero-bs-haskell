const todoFromPartial = (tId, input) => {
  return {
    title: input.title || "",
    completed: false,
    url: "http://localhost:7879/api/" + tId,
    todoId: tId,
    order: input.order
  }
}

module.exports = {
  initialState: {
    todos: {},
    nextIndex: 1
  },

  createTodo: (state, input) => {
    const index = state.nextIndex
    const tId = "" + index
    const newTodo = todoFromPartial(tId, input)
    const newState = {
      todos: { ...state.todos, [tId]: newTodo },
      nextIndex: index + 1
    }
    return [newState, newTodo]
  },

  updateTodo: (state, existing, input) => {
    const newState = { ...state.todos, [existing.todoId]: updated }

    const updated = {
      ...existing,
      title: input.title || existing.title,
      completed: input.completed || existing.completed,
      order: input.order
    }

    return [newState, updated]
  },

  deleteTodo: (state, todo) => {
    delete state.todos[todo.todoId]
    return state
  },

  findTodo: (state, tId) => {
    return state.todos[tId]
  },

  stateToList: state => {
    return Object.values(state.todos)
  }
}
