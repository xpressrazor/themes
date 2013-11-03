#!/usr/bin/perl

# network_restart.pl
# A network restarting script.

# We want to check $loop times before exiting, with $sleep seconds delay
# If network is bad, we don't get connection immediately, 
# so we must try for some durations before we get any connection

# You can run this script using cron at specific hours or even minutes of day
# $ sudo crontab -e
# m h dom mon dow command
# */2 * * * * sudo perl /home/username/bin/network_restart.pl
# NOTE: In above command don't put # (comment). It runs every 2 minutes.
# To run every hour user above command would be
# * */1 * * * sudo perl /home/username/bin/network_restart.pl


# Change these values to suit your requirement
$loop = 10;				
$sleep = "35s";		# Make sure its more than 30s.
$connected = "no";

for ($i = 0; $i < $loop; $i++) {	
	$command = `nm-tool | grep State`;	# command to run to get "connected"" string
	
	foreach ($command) {
		chomp;
		@states = split /\s+/;
		
		foreach (@states) {
			chomp;
			
			#print "|" . $_ . "|\n";			
			if ($_ eq "connected") {	# brackets are freaking ugly				
				$connected = "yes";
			}
    	}
	}	

	if ($connected ne "yes") { 
	    system "service network-manager restart";
        system "sleep $sleep";	# prepare for next loop (check)		        
        print "checking " . ($i + 1) . " times\n";
	} else {					# If we got the connection we can safely exit		
		print "we got a connection!!!\n";
		last;
	}	
}

if ($connected eq "no") {
    print "sorry could not connect this time\n";
}

