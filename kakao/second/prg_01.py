def solution(fromBox, ball, numA, numB):
  boxes = [numA, numB]
  get_giver_idx = lambda box: 0 if box == 'A' else 1
  get_receiver_idx = lambda box: 1 if box == 'A' else 0

  for from_box, num_of_ball in zip(fromBox, ball):
    g_idx, r_idx = get_giver_idx(from_box), get_receiver_idx(from_box)
    giver, receiver = boxes[g_idx], boxes[r_idx]

    if giver < num_of_ball:
      continue

    if receiver + num_of_ball > 1000:
      num_of_ball = 1000 - receiver

    boxes[g_idx] -= num_of_ball
    boxes[r_idx] += num_of_ball

  return boxes
