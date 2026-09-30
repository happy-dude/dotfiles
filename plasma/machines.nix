# Per-machine Plasma values, keyed by username.
#
# KWin matches input devices by libinput name and vendor/product ID:
#   grep -H . /sys/class/input/event*/device/{name,id/vendor,id/product}
# Escape "/" in a device name as "\\/"; plasma-manager splits groups on it.
# Output scales stay in kwinoutputconfig.json, which KWin rewrites on every
# display change.
{
  schan = {
    xwaylandScale = 1.25;
    touchpads = [
      {
        name = "SNSL002D:00 2C2F:002D Touchpad";
        vendorId = "2c2f";
        productId = "002d";
        pointerSpeed = 0.8;
        naturalScroll = true;
      }
    ];
    mice = [
      {
        name = "SNSL002D:00 2C2F:002D Mouse";
        vendorId = "2c2f";
        productId = "002d";
        acceleration = 1.0;
        naturalScroll = true;
      }
      {
        name = "Logitech MX Vertical";
        vendorId = "046d";
        productId = "407b";
        acceleration = 1.0;
        naturalScroll = true;
        scrollSpeed = 2;
      }
      {
        name = "TPPS\\/2 Elan TrackPoint";
        vendorId = "0002";
        productId = "000a";
        acceleration = 1.0;
        naturalScroll = true;
      }
    ];
  };
}
