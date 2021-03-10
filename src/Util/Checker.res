let check = (isTerminated, next, state) => {
  let rec run = state => {
    state->isTerminated ? state : run(state->next)
  }
  run(state)
}
