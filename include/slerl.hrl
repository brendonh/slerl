%%%-------------------------------------------------------------------
%%% File    : slerl.hrl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : 
%%%
%%% Created :  7 Feb 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------


%% Sim connections

-record(sim, {
  ip,
  port,
  circuitCode,
  regionPos,
  seedCapability,
  agentID,
  sessionID
}).



-record(message, {
  spec,
  message,
  zerocoded,
  reliable=false,
  resend=false,
  extra
}).



%% Message definitions

-record(messageDef, {
  name,
  frequency,
  number,
  messageID,
  trusted,
  zerocoded,
  flag,
  blocks
}).

-record(blockDef, {
  name,
  quantity,
  repeats,
  parameters
}).

