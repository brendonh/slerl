%%%-------------------------------------------------------------------
%%% File    : slerl_codes.erl
%%% Author  : Brendon Hogger <brendonh@dev.brendonh.org>
%%% Description : Code conversions for various message types
%%%
%%% Created : 14 Feb 2010 by Brendon Hogger <brendonh@dev.brendonh.org>
%%%-------------------------------------------------------------------
-module(slerl_codes).

-compile([export_all]).

convert_chat_code(0) -> whisper;
convert_chat_code(1) -> say;
convert_chat_code(2) -> shout;
convert_chat_code(Other) -> {unknown, Other}.

convert_chat_type(whisper) -> 0;
convert_chat_type(normal) -> 1;
convert_chat_type(shout) -> 2.


convert_im_type(0) -> message_from_agent;
convert_im_type(1) -> message_box;
convert_im_type(3) -> group_invitation;
convert_im_type(4) -> inventory_offered;
convert_im_type(5) -> inventory_accepted;
convert_im_type(6) -> inventory_declined;
convert_im_type(7) -> group_vote;
convert_im_type(9) -> task_inventory_offered;
convert_im_type(10) -> task_inventory_accepted;
convert_im_type(11) -> task_inventory_declined;
convert_im_type(12) -> new_user_default;
convert_im_type(13) -> session_add;
convert_im_type(14) -> session_offline_add;
convert_im_type(15) -> session_group_start;
convert_im_type(16) -> session_cardless_start;
convert_im_type(17) -> session_send;
convert_im_type(18) -> session_drop;
convert_im_type(19) -> message_from_object;
convert_im_type(20) -> busy_auto_response;
convert_im_type(21) -> console_and_chat_history;
convert_im_type(22) -> request_teleport;
convert_im_type(23) -> accept_teleport;
convert_im_type(24) -> deny_teleport;
convert_im_type(25) -> god_like_request_teleport;
convert_im_type(26) -> currently_unused;
convert_im_type(28) -> goto_url;
convert_im_type(29) -> session911_start;
convert_im_type(30) -> lure911;
convert_im_type(31) -> from_task_as_alert;
convert_im_type(32) -> group_notice;
convert_im_type(33) -> group_notice_inventory_accepted;
convert_im_type(34) -> group_notice_inventory_declined;
convert_im_type(35) -> group_invitation_accept;
convert_im_type(36) -> group_invitation_decline;
convert_im_type(37) -> group_notice_requested;
convert_im_type(38) -> friendship_offered;
convert_im_type(39) -> friendship_accepted;
convert_im_type(40) -> friendship_declined;
convert_im_type(41) -> start_typing;
convert_im_type(42) -> stop_typing;
convert_im_type(Other) -> {unknown, Other}.
