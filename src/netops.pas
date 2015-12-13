{ Modified from
 http://free-pascal-general.1045716.n5.nabble.com/lNet-getting-the-local-IP-td3200339.html }

program GetPrimaryIpAddress; 
{$mode objfpc} 

uses 
  baseunix, 
  unixtype, 
  sockets, 
  SysUtils; 

procedure Get(out AddrOut: string); 
const 
  CN_GDNS_ADDR = '127.0.0.1'; 
  CN_GDNS_PORT = 53; 
var 
  sock: longint; 
  err: longint; 
  UnixAddr: TInetSockAddr; 
  HostAddr: TSockAddr; 
  len: Integer; 
begin 
  err := 0; 

  sock := fpsocket(AF_INET, SOCK_DGRAM, 0); 
  assert(sock <> -1); 

// changed because previous properties were deprecated 
  UnixAddr.sin_family := AF_INET; 
  UnixAddr.sin_port := htons(CN_GDNS_PORT); 
  UnixAddr.sin_addr := StrToHostAddr(CN_GDNS_ADDR); 

  if (fpConnect(sock, @UnixAddr, SizeOf(UnixAddr)) = 0) then 
  begin 
    try 
      len := SizeOf(HostAddr); 
      if (fpgetsockname(sock, @HostAddr, @len) = 0) then 
      begin 
        AddrOut := NetAddrToStr(HostAddr.sin_addr); 
      end 
      else 
      begin 
        err:=socketError; 
      end; 
    finally 
      if (fpclose(sock) <> 0) then 
      begin 
        err := socketError; 
      end; 
    end; 
  end 
  else 
  begin 
    err:=socketError; 
  end; 

  if (err <> 0) then 
  begin 
    // report error 
  end; 
end; 

var 
  strAddr: string; 

begin 
  Get(strAddr); 
  WriteLn('ip : ',strAddr); 
end. 
_________
