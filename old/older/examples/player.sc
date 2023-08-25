(
  var buffers, response, sampledir;
  sampledir = "/home/alex/Dropbox/projects/dirt/samples/";
  SynthDef('player',
           { arg out=0, bufnum=0, rate=1, pan=0, ts=1, crackle=1, browndel=0;
             var x, y, buffer, delayedSignal, mixedSignal;
             x = PlayBuf.ar(1, bufnum, rate);
			 x = Pan2.ar(x, pan);
             x = x + if (crackle > 1.5, Crackle.ar(crackle), 0);
             x = if (browndel > 0,
                     DelayN.ar(x, 0.02, 0.02, 1, BrownNoise.ar(x.distort, 0) * browndel),
                     x
                    );
             x = EnvGen.kr(Env.perc, doneAction: 2,
				     timeScale: ts) * x;
			 Out.ar(0, x);
           }
          ).send(s);

  buffers = Dictionary.new;


  response = {
    arg time, responder, message;
    var mybuf, sample, pan, crackle, noise, ts, browndel, rate;
    sample   = message[1];
    pan      = message[2];
    crackle  = message[3];
    noise    = message[4];
    ts       = message[5];
    browndel = message[6];
    rate     = message[7];

	"aha".postln;

	sample = sampledir ++ sample;
	sample.postln;

    mybuf = buffers.at(sample);
    if (mybuf == nil,
      {mybuf = Buffer.read(s, sample);
       buffers.add(sample -> mybuf);
      }
    );
    time.postln;
	Date.getDate.rawSeconds.postln;
    SystemClock.sched(time - Date.getDate.rawSeconds,
          {Synth.new("player", [
				\pan,  pan,
				\crackle, crackle,
				\browndel, browndel,
			    \ts, ts,
				\rate, rate,
				\out,
				0,
				\bufnum, mybuf.bufnum
               ]
		     );
		    nil
          }
	)
  };

  o = OSCresponder(nil, '/player', response);
  o.add;
)

OSCFunc.trace(true);

NetAddr.langPort;
