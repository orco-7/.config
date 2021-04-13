from libqtile import widget, bar
from colors import colors, colors2
import qtile

widget_default = dict(
        font = 'Monospace Bold',
        fontsize = 14,
        padding = 3,
        background = colors[1],
        )

top1 = bar.Bar(
        [
            widget.Image(
                filename = '~/.config/qtile/icons/arch.png'
                ),
            widget.CurrentLayout(
                foreground = colors[5],
                ),
            widget.GroupBox(
                rounded = True,
                active = colors[3],
                inactive = colors[2],
                highlight_color = colors[1],
                highlight_method = 'line',
                hide_unused = True,
                this_current_screen_border = colors[6],
                ),
            widget.Prompt(foreground = colors[6]),
            widget.WindowName(
                foreground = colors[6],
                ),

            widget.TextBox(
                text = 'Vol: ',
                foreground = colors[5],
                mouse_callbacks = {'Button3': lambda: qtile.cmd_spawn('pavucontrol')}
                ),
            widget.PulseVolume(
                foreground = colors[5],
                ),
            widget.Battery(
                foreground = colors[6],
                format = '{percent:2.0%} {watt:.2f} W',
                mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn('xfce4-power-manager-settings')}
                ),
            widget.Systray(),
            widget.Clock(
                format = '%a %H:%M:%S %d/%m/%Y',
                foreground = colors[5],
                ),
            widget.TextBox(
                text = 'Simone',
                foreground = colors[6],
                mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn('./scripts/power_menu.sh')}
                ),
            ],
        24,
        margin = [7, 7, 7, 7],
    )



bottom1 = bar.Bar(
        [
            widget.KhalCalendar(
                foreground = colors[5],
                max_chars = 10,
                ),
            widget.Spacer(),
            widget.Image(
                filename = '~/.config/qtile/icons/computer-chip-8.png',
                ),
            widget.ThermalSensors(
                foreground = colors[5],
                tag_sensors = 'Core 0',
                ),
            widget.CPU(
                foreground = colors[5],
                ),
            widget.Image(
                filename = '~/.config/qtile/icons/computer-ram.png',
                ),
            widget.Memory(
                foreground = colors[6],
                format = '{MemUsed:.0f} M',
                ),
            widget.Image(
                filename = '~/.config/qtile/icons/network-arrow-sync.png',
                ),
            widget.Net(
                foreground = colors[5],
                format = 'd:{down} u:{up}',
                ),
            ],
        24,
        margin = [5, 5, 5, 5],
        )


top2 = bar.Bar(
        [
            widget.Image(
                filename = '~/.config/qtile/icons/arch.png',
                background = colors2[0],
                ),
            widget.CurrentLayout(
                foreground = colors2[1],
                background = colors2[0],
                ),
            widget.GroupBox(
                rounded = True,
                active = colors2[2],
                inactive = colors2[2],
                highlight_color = colors2[1],
                highlight_method = 'line',
                hide_unused = True,
                this_current_screen_border = colors2[1],
                background = colors2[0],
                ),
            widget.Prompt(
                background = colors2[0],
                foreground = colors2[1],
                ),
            widget.WindowName(
                background = colors2[0],
                foreground = colors2[1],
                ),
            widget.Battery(
                foreground = colors2[2],
                background = colors2[0],
                format = '{percent:2.0%} {watt:.2f} W',
                mouse_callbacks = {'Button1': lambda: qtile.cmd_spawn('xfce4-power-manager-settings'),},
                ),
            widget.Clock(
                format = '%a %H:%M:S %d/%m/%Y',
                foreground = colors2[1],
                background = colors2[0],
                ),
            widget.Mpd2(
                background = colors2[0],
                foreground = colors2[2],
                colors_progress = colors2[2],
                ),
            ],
        24,
        background = colors2[0],
        margin = [5, 5, 5, 5],
        )

bottom2 = bar.Bar(
        [
            widget.Image(
                filename = '~/.config/qtile/icons/hard-drive.png',
                background = colors2[0],
                        ),
            widget.DF(
                warn_space = 1000,
                warn_color = colors2[1],
                background = colors2[0],
                format = '({uf}{m}|{r:.0f}%)',
                     ),
            widget.Image(
                background = colors2[0],
                filename = '~/.config/qtile/icons/turntable.png',
                        ),
            widget.DF(
                warn_space = 1000,
                warn_color = colors2[2],
                format = '({uf}{m}|{r:.0f}%)',
                partition = '/backup',
                background = colors2[0],
                     ),
            widget.DF(
                warn_space = 1000,
                warn_color = colors2[1],
                format = '({uf}{m}|{r:.0f}%)',
                partition = '/home/simone/storage',
                background = colors2[0],
                     ),

            widget.Spacer(background = colors2[0]),
            widget.Spacer(background = colors2[0]),
            widget.NvidiaSensors(
                foreground = colors2[2],
                format = '{temp}°C {perf}',
                background = colors2[0]
                ),
            widget.Image(
                filename = '~/.config/qtile/icons/computer-chip-8.png',
                background = colors2[0],
                        ),
            widget.ThermalSensor(
                    foreground = colors2[1],
                    tag_sensor = 'Core 0',
                    background = colors2[0],
                    ),
            widget.CPU(
                foreground = colors2[2],
                background = colors2[0],
                      ),
            widget.Image(
                background = colors2[0],
                filename = '~/.config/qtile/icons/computer-ram.png',
                        ),
            widget.Memory(
                foreground = colors2[1],
                background = colors2[0],
                format = '{MemUsed:.0f} M',
                         ),
            widget.Sep(
                background = colors2[0],
                foreground = colors2[2],
                linewidth = 3,
                      ),
            widget.Image(
                background = colors2[0],
                filename = '~/.config/qtile/icons/network-arrow-sync.png',
                        ),
            widget.Net(
                foreground = colors2[1],
                background = colors2[0],
                format = 'd:{down} u:{up}',
                      ),

            ],
            24,
            background = colors2[0],
            margin = [5, 5, 5, 5],
            )


