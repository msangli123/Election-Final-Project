plot_grayscale = function(image_array, label = NULL, class_names = NULL){
  if(!is.null(label) & !is.null(class_names)){
    title = class_names %>% filter(class == !!label) %>% pull(name)
  } else{
    title = label
  }
  p = image_array %>% 
    as.data.frame() %>% 
    rownames_to_column(var = "row") %>% 
    mutate_all(as.numeric) %>%
    pivot_longer(-row, 
                 values_to = "intensity", 
                 names_to = "col", 
                 names_prefix = "V", 
                 names_transform = list(col = as.integer)) %>% 
    mutate(row = as.integer(row)) %>% 
    ggplot(aes(x = col, y = - row, fill = intensity)) + 
    geom_tile() + 
    coord_fixed() + 
    scale_x_continuous(expand = c(0,0)) + 
    scale_y_continuous(expand = c(0,0)) + 
    scale_fill_gradient(low = "white", high = "black") + 
    theme_void() + 
    theme(panel.border = element_rect(colour = "black", fill=NA),
          plot.title = element_text(hjust = 0.5),
          legend.position = "none")  
  
  if(!is.null(title)){
    p = p + ggtitle(title)
  }
  
  p
}

plot_confusion_matrix = function(predicted_responses, actual_responses, class_names = NULL){
  if(is.null(class_names)){
    rotate_angle = 0
    h_just = 0.5
    classes = sort(unique(c(predicted_responses, actual_responses)))
    class_names = tibble(class = classes, name = classes)
  } else{
    rotate_angle = 45
    h_just = 0
  }
  tibble(predicted_response = predicted_responses,
         actual_response = actual_responses) %>%
    table() %>%
    as_tibble() %>%
    ggplot(aes(x = predicted_response, y = actual_response, label = n, fill = n)) +
    geom_tile(alpha = 0.5) + 
    geom_text() +
    scale_x_discrete(position = "top", 
                     expand = c(0,0), 
                     breaks = class_names$class, 
                     labels = class_names$name) +
    scale_y_discrete(limits = rev, 
                     expand = c(0,0),
                     breaks = class_names$class,
                     labels = class_names$name) +
    scale_fill_gradient(low = "blue", high = "red") +
    coord_fixed() +
    labs(x = "Predicted Response",
         y = "Actual Response") + 
    theme_bw() + 
    theme(panel.grid = element_blank(),
          legend.position = "none",
          axis.text.x = element_text(angle = rotate_angle, vjust = 0.5, hjust=h_just))
}

plot_model_history = function(history){
  epochs = 1:length(history$loss)
  bind_rows(tibble(epoch = epochs,
                   metric = "loss",
                   value = history$loss,
                   set = "training"),
            tibble(epoch = epochs,
                   metric = "accuracy",
                   value = history$accuracy,
                   set = "training"),
            tibble(epoch = epochs,
                   metric = "loss",
                   value = history$val_loss,
                   set = "validation"),
            tibble(epoch = epochs,
                   metric = "accuracy",
                   value = history$val_accuracy,
                   set = "validation")
  ) %>%
    mutate(metric = factor(metric, levels = c("loss", "accuracy"))) %>%
    ggplot(aes(x = epoch, y = value, colour = set)) +
    geom_line() + 
    geom_point() + 
    facet_grid(metric ~ ., scales = "free") + 
    labs(x = "Epoch") +
    theme_bw() + 
    theme(legend.position = "bottom",
          legend.title = element_blank(),
          axis.title.y = element_blank())
}
