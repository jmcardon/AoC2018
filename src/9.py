

class Node:
    def __init__(self, value: int, next = None, prev = None):
        self.value = value
        self.next = next
        self.prev = prev
    def set_next(self, n):
        self.next = n
    def set_prev(self, n):
        self.prev = n
    def set_val(value: int):
        self.value = value
    def __str__(self):
        return "Node[" + str(self.value) + "]"

class LinkedList:
    def __init__(self):
        self.head = Node(0, None)
        self.head.set_next(self.head)
        self.head.set_prev(self.head)
        self.current = self.head
    def set_current(self, n):
        self.current = n
    def insert_right(self, n):
        n.set_prev(self.current)
        n.set_next(self.current.next)
        self.current.next.set_prev(n)
        self.current.set_next(n)
        self.current = n
    def cycle_right(self, i):
        for unused in range(i):
            self.current = self.current.next
    def cycle_left(self, i):
       for unused in range(i):
            self.current = self.current.prev
    def remove_current(self):
        n = self.current
        self.current.next.set_prev(self.current.prev)
        self.current.prev.set_next(self.current.next)
        self.current = self.current.next
        if n == self.head:
            self.head = self.current
    def __str__(self):
        s_curr = self.head
        x = "[" + str(s_curr)
        s_curr = s_curr.next
        while (s_curr != self.head):
            if s_curr == self.current:
                x = x + ",(" + str(s_curr) + ")"
            else:
                x = x + "," + str(s_curr)
            s_curr = s_curr.next
        x += "]"
        return x

x = LinkedList()

last_marble = 72059*100 # uncomment for part 2
# last_marble = 72059 #comment for part 2
players_amount = 411
player_dict = {}
player_ix = 1
ctr = 0
while (ctr <= last_marble):
    ctr += 1
    if (ctr % 23) != 0:
        x.cycle_right(1)
        x.insert_right(Node(int(ctr)))
    else:
        if not player_ix in player_dict:
            player_dict[player_ix] = 0
        player_dict[player_ix] += ctr
        x.cycle_left(7)
        player_dict[player_ix] += x.current.value
        x.remove_current()
    player_ix = (player_ix + 1) % players_amount

print(max(player_dict.values()))

