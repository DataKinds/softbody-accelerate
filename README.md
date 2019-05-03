# softbody-accelerate

What I had to change:

~~* Squared the restoring factor (v_a') before linear interpolation~~
~~* No longer using euler integration on thing~~
* Need a way for system to lose energy
  * Hmm.
  * Introduced energyBleed with no physical correspondence. But, it looks good!
