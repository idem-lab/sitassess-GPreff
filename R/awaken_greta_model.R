awaken_greta_model <- function (model) {

    # there is no log prob function defined (dead pointer) and a few other things
    model$dag$tf_log_prob_function

    # force rebuild these things, like we do for parallel things
    dag <- model$dag
    dag$define_tf_trace_values_batch()
    dag$define_tf_log_prob_function()

}
