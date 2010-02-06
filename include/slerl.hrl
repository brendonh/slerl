%%%-------------------------------------------------------------------
%%% File    : slerl.hrl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : 
%%%
%%% Created :  7 Feb 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------

-record(messageDef, {
          name,
          frequency,
          number,
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

