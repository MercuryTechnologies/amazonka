{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.GameLift.Types.GameSessionPlacement
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.GameSessionPlacement where

import qualified Amazonka.Core as Core
import Amazonka.GameLift.Types.GameProperty
import Amazonka.GameLift.Types.GameSessionPlacementState
import Amazonka.GameLift.Types.PlacedPlayerSession
import Amazonka.GameLift.Types.PlayerLatency
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Object that describes a StartGameSessionPlacement request. This object
-- includes the full details of the original request plus the current
-- status and start\/end time stamps.
--
-- Game session placement-related operations include:
--
-- -   StartGameSessionPlacement
--
-- -   DescribeGameSessionPlacement
--
-- -   StopGameSessionPlacement
--
-- /See:/ 'newGameSessionPlacement' smart constructor.
data GameSessionPlacement = GameSessionPlacement'
  { -- | Current status of the game session placement request.
    --
    -- -   __PENDING__ -- The placement request is currently in the queue
    --     waiting to be processed.
    --
    -- -   __FULFILLED__ -- A new game session and player sessions (if
    --     requested) have been successfully created. Values for
    --     /GameSessionArn/ and /GameSessionRegion/ are available.
    --
    -- -   __CANCELLED__ -- The placement request was canceled with a call to
    --     StopGameSessionPlacement.
    --
    -- -   __TIMED_OUT__ -- A new game session was not successfully created
    --     before the time limit expired. You can resubmit the placement
    --     request as needed.
    --
    -- -   __FAILED__ -- GameLift is not able to complete the process of
    --     placing the game session. Common reasons are the game session
    --     terminated before the placement process was completed, or an
    --     unexpected internal error.
    status :: Prelude.Maybe GameSessionPlacementState,
    -- | A unique identifier for a game session placement.
    placementId :: Prelude.Maybe Prelude.Text,
    -- | A set of custom properties for a game session, formatted as key:value
    -- pairs. These properties are passed to a game server process in the
    -- GameSession object with a request to start a new game session (see
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
    gameProperties :: Prelude.Maybe [GameProperty],
    -- | The IP address of the game session. To connect to a GameLift game
    -- server, an app needs both the IP address and port number. This value is
    -- set once the new game session is placed (placement status is
    -- @FULFILLED@).
    ipAddress :: Prelude.Maybe Prelude.Text,
    -- | A descriptive label that is associated with a game session. Session
    -- names do not need to be unique.
    gameSessionName :: Prelude.Maybe Prelude.Text,
    -- | Time stamp indicating when this request was placed in the queue. Format
    -- is a number expressed in Unix time as milliseconds (for example
    -- @\"1469498468.057\"@).
    startTime :: Prelude.Maybe Core.POSIX,
    -- | A unique identifier for the game session. This value is set once the new
    -- game session is placed (placement status is @FULFILLED@).
    gameSessionId :: Prelude.Maybe Prelude.Text,
    -- | Name of the Region where the game session created by this placement
    -- request is running. This value is set once the new game session is
    -- placed (placement status is @FULFILLED@).
    gameSessionRegion :: Prelude.Maybe Prelude.Text,
    -- | Information on the matchmaking process for this game. Data is in JSON
    -- syntax, formatted as a string. It identifies the matchmaking
    -- configuration used to create the match, and contains data on all players
    -- assigned to the match, including player attributes and team assignments.
    -- For more details on matchmaker data, see
    -- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-server.html#match-server-data Match Data>.
    matchmakerData :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of players that can be connected simultaneously to
    -- the game session.
    maximumPlayerSessionCount :: Prelude.Maybe Prelude.Natural,
    -- | Time stamp indicating when this request was completed, canceled, or
    -- timed out.
    endTime :: Prelude.Maybe Core.POSIX,
    -- | Identifier for the game session created by this placement request. This
    -- value is set once the new game session is placed (placement status is
    -- @FULFILLED@). This identifier is unique across all Regions. You can use
    -- this value as a @GameSessionId@ value as needed.
    gameSessionArn :: Prelude.Maybe Prelude.Text,
    -- | A set of values, expressed in milliseconds, that indicates the amount of
    -- latency that a player experiences when connected to AWS Regions.
    playerLatencies :: Prelude.Maybe [PlayerLatency],
    -- | A set of custom game session properties, formatted as a single string
    -- value. This data is passed to a game server process in the GameSession
    -- object with a request to start a new game session (see
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
    gameSessionData :: Prelude.Maybe Prelude.Text,
    -- | The DNS identifier assigned to the instance that is running the game
    -- session. Values have the following format:
    --
    -- -   TLS-enabled fleets:
    --     @\<unique identifier>.\<region identifier>.amazongamelift.com@.
    --
    -- -   Non-TLS-enabled fleets:
    --     @ec2-\<unique identifier>.compute.amazonaws.com@. (See
    --     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-instance-addressing.html#concepts-public-addresses Amazon EC2 Instance IP Addressing>.)
    --
    -- When connecting to a game session that is running on a TLS-enabled
    -- fleet, you must use the DNS name, not the IP address.
    dnsName :: Prelude.Maybe Prelude.Text,
    -- | A descriptive label that is associated with game session queue. Queue
    -- names must be unique within each Region.
    gameSessionQueueName :: Prelude.Maybe Prelude.Text,
    -- | A collection of information on player sessions created in response to
    -- the game session placement request. These player sessions are created
    -- only once a new game session is successfully placed (placement status is
    -- @FULFILLED@). This information includes the player ID (as provided in
    -- the placement request) and the corresponding player session ID. Retrieve
    -- full player sessions by calling DescribePlayerSessions with the player
    -- session ID.
    placedPlayerSessions :: Prelude.Maybe [PlacedPlayerSession],
    -- | The port number for the game session. To connect to a GameLift game
    -- server, an app needs both the IP address and port number. This value is
    -- set once the new game session is placed (placement status is
    -- @FULFILLED@).
    port :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GameSessionPlacement' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'gameSessionPlacement_status' - Current status of the game session placement request.
--
-- -   __PENDING__ -- The placement request is currently in the queue
--     waiting to be processed.
--
-- -   __FULFILLED__ -- A new game session and player sessions (if
--     requested) have been successfully created. Values for
--     /GameSessionArn/ and /GameSessionRegion/ are available.
--
-- -   __CANCELLED__ -- The placement request was canceled with a call to
--     StopGameSessionPlacement.
--
-- -   __TIMED_OUT__ -- A new game session was not successfully created
--     before the time limit expired. You can resubmit the placement
--     request as needed.
--
-- -   __FAILED__ -- GameLift is not able to complete the process of
--     placing the game session. Common reasons are the game session
--     terminated before the placement process was completed, or an
--     unexpected internal error.
--
-- 'placementId', 'gameSessionPlacement_placementId' - A unique identifier for a game session placement.
--
-- 'gameProperties', 'gameSessionPlacement_gameProperties' - A set of custom properties for a game session, formatted as key:value
-- pairs. These properties are passed to a game server process in the
-- GameSession object with a request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
--
-- 'ipAddress', 'gameSessionPlacement_ipAddress' - The IP address of the game session. To connect to a GameLift game
-- server, an app needs both the IP address and port number. This value is
-- set once the new game session is placed (placement status is
-- @FULFILLED@).
--
-- 'gameSessionName', 'gameSessionPlacement_gameSessionName' - A descriptive label that is associated with a game session. Session
-- names do not need to be unique.
--
-- 'startTime', 'gameSessionPlacement_startTime' - Time stamp indicating when this request was placed in the queue. Format
-- is a number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
--
-- 'gameSessionId', 'gameSessionPlacement_gameSessionId' - A unique identifier for the game session. This value is set once the new
-- game session is placed (placement status is @FULFILLED@).
--
-- 'gameSessionRegion', 'gameSessionPlacement_gameSessionRegion' - Name of the Region where the game session created by this placement
-- request is running. This value is set once the new game session is
-- placed (placement status is @FULFILLED@).
--
-- 'matchmakerData', 'gameSessionPlacement_matchmakerData' - Information on the matchmaking process for this game. Data is in JSON
-- syntax, formatted as a string. It identifies the matchmaking
-- configuration used to create the match, and contains data on all players
-- assigned to the match, including player attributes and team assignments.
-- For more details on matchmaker data, see
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-server.html#match-server-data Match Data>.
--
-- 'maximumPlayerSessionCount', 'gameSessionPlacement_maximumPlayerSessionCount' - The maximum number of players that can be connected simultaneously to
-- the game session.
--
-- 'endTime', 'gameSessionPlacement_endTime' - Time stamp indicating when this request was completed, canceled, or
-- timed out.
--
-- 'gameSessionArn', 'gameSessionPlacement_gameSessionArn' - Identifier for the game session created by this placement request. This
-- value is set once the new game session is placed (placement status is
-- @FULFILLED@). This identifier is unique across all Regions. You can use
-- this value as a @GameSessionId@ value as needed.
--
-- 'playerLatencies', 'gameSessionPlacement_playerLatencies' - A set of values, expressed in milliseconds, that indicates the amount of
-- latency that a player experiences when connected to AWS Regions.
--
-- 'gameSessionData', 'gameSessionPlacement_gameSessionData' - A set of custom game session properties, formatted as a single string
-- value. This data is passed to a game server process in the GameSession
-- object with a request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
--
-- 'dnsName', 'gameSessionPlacement_dnsName' - The DNS identifier assigned to the instance that is running the game
-- session. Values have the following format:
--
-- -   TLS-enabled fleets:
--     @\<unique identifier>.\<region identifier>.amazongamelift.com@.
--
-- -   Non-TLS-enabled fleets:
--     @ec2-\<unique identifier>.compute.amazonaws.com@. (See
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-instance-addressing.html#concepts-public-addresses Amazon EC2 Instance IP Addressing>.)
--
-- When connecting to a game session that is running on a TLS-enabled
-- fleet, you must use the DNS name, not the IP address.
--
-- 'gameSessionQueueName', 'gameSessionPlacement_gameSessionQueueName' - A descriptive label that is associated with game session queue. Queue
-- names must be unique within each Region.
--
-- 'placedPlayerSessions', 'gameSessionPlacement_placedPlayerSessions' - A collection of information on player sessions created in response to
-- the game session placement request. These player sessions are created
-- only once a new game session is successfully placed (placement status is
-- @FULFILLED@). This information includes the player ID (as provided in
-- the placement request) and the corresponding player session ID. Retrieve
-- full player sessions by calling DescribePlayerSessions with the player
-- session ID.
--
-- 'port', 'gameSessionPlacement_port' - The port number for the game session. To connect to a GameLift game
-- server, an app needs both the IP address and port number. This value is
-- set once the new game session is placed (placement status is
-- @FULFILLED@).
newGameSessionPlacement ::
  GameSessionPlacement
newGameSessionPlacement =
  GameSessionPlacement'
    { status = Prelude.Nothing,
      placementId = Prelude.Nothing,
      gameProperties = Prelude.Nothing,
      ipAddress = Prelude.Nothing,
      gameSessionName = Prelude.Nothing,
      startTime = Prelude.Nothing,
      gameSessionId = Prelude.Nothing,
      gameSessionRegion = Prelude.Nothing,
      matchmakerData = Prelude.Nothing,
      maximumPlayerSessionCount = Prelude.Nothing,
      endTime = Prelude.Nothing,
      gameSessionArn = Prelude.Nothing,
      playerLatencies = Prelude.Nothing,
      gameSessionData = Prelude.Nothing,
      dnsName = Prelude.Nothing,
      gameSessionQueueName = Prelude.Nothing,
      placedPlayerSessions = Prelude.Nothing,
      port = Prelude.Nothing
    }

-- | Current status of the game session placement request.
--
-- -   __PENDING__ -- The placement request is currently in the queue
--     waiting to be processed.
--
-- -   __FULFILLED__ -- A new game session and player sessions (if
--     requested) have been successfully created. Values for
--     /GameSessionArn/ and /GameSessionRegion/ are available.
--
-- -   __CANCELLED__ -- The placement request was canceled with a call to
--     StopGameSessionPlacement.
--
-- -   __TIMED_OUT__ -- A new game session was not successfully created
--     before the time limit expired. You can resubmit the placement
--     request as needed.
--
-- -   __FAILED__ -- GameLift is not able to complete the process of
--     placing the game session. Common reasons are the game session
--     terminated before the placement process was completed, or an
--     unexpected internal error.
gameSessionPlacement_status :: Lens.Lens' GameSessionPlacement (Prelude.Maybe GameSessionPlacementState)
gameSessionPlacement_status = Lens.lens (\GameSessionPlacement' {status} -> status) (\s@GameSessionPlacement' {} a -> s {status = a} :: GameSessionPlacement)

-- | A unique identifier for a game session placement.
gameSessionPlacement_placementId :: Lens.Lens' GameSessionPlacement (Prelude.Maybe Prelude.Text)
gameSessionPlacement_placementId = Lens.lens (\GameSessionPlacement' {placementId} -> placementId) (\s@GameSessionPlacement' {} a -> s {placementId = a} :: GameSessionPlacement)

-- | A set of custom properties for a game session, formatted as key:value
-- pairs. These properties are passed to a game server process in the
-- GameSession object with a request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
gameSessionPlacement_gameProperties :: Lens.Lens' GameSessionPlacement (Prelude.Maybe [GameProperty])
gameSessionPlacement_gameProperties = Lens.lens (\GameSessionPlacement' {gameProperties} -> gameProperties) (\s@GameSessionPlacement' {} a -> s {gameProperties = a} :: GameSessionPlacement) Prelude.. Lens.mapping Lens.coerced

-- | The IP address of the game session. To connect to a GameLift game
-- server, an app needs both the IP address and port number. This value is
-- set once the new game session is placed (placement status is
-- @FULFILLED@).
gameSessionPlacement_ipAddress :: Lens.Lens' GameSessionPlacement (Prelude.Maybe Prelude.Text)
gameSessionPlacement_ipAddress = Lens.lens (\GameSessionPlacement' {ipAddress} -> ipAddress) (\s@GameSessionPlacement' {} a -> s {ipAddress = a} :: GameSessionPlacement)

-- | A descriptive label that is associated with a game session. Session
-- names do not need to be unique.
gameSessionPlacement_gameSessionName :: Lens.Lens' GameSessionPlacement (Prelude.Maybe Prelude.Text)
gameSessionPlacement_gameSessionName = Lens.lens (\GameSessionPlacement' {gameSessionName} -> gameSessionName) (\s@GameSessionPlacement' {} a -> s {gameSessionName = a} :: GameSessionPlacement)

-- | Time stamp indicating when this request was placed in the queue. Format
-- is a number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
gameSessionPlacement_startTime :: Lens.Lens' GameSessionPlacement (Prelude.Maybe Prelude.UTCTime)
gameSessionPlacement_startTime = Lens.lens (\GameSessionPlacement' {startTime} -> startTime) (\s@GameSessionPlacement' {} a -> s {startTime = a} :: GameSessionPlacement) Prelude.. Lens.mapping Core._Time

-- | A unique identifier for the game session. This value is set once the new
-- game session is placed (placement status is @FULFILLED@).
gameSessionPlacement_gameSessionId :: Lens.Lens' GameSessionPlacement (Prelude.Maybe Prelude.Text)
gameSessionPlacement_gameSessionId = Lens.lens (\GameSessionPlacement' {gameSessionId} -> gameSessionId) (\s@GameSessionPlacement' {} a -> s {gameSessionId = a} :: GameSessionPlacement)

-- | Name of the Region where the game session created by this placement
-- request is running. This value is set once the new game session is
-- placed (placement status is @FULFILLED@).
gameSessionPlacement_gameSessionRegion :: Lens.Lens' GameSessionPlacement (Prelude.Maybe Prelude.Text)
gameSessionPlacement_gameSessionRegion = Lens.lens (\GameSessionPlacement' {gameSessionRegion} -> gameSessionRegion) (\s@GameSessionPlacement' {} a -> s {gameSessionRegion = a} :: GameSessionPlacement)

-- | Information on the matchmaking process for this game. Data is in JSON
-- syntax, formatted as a string. It identifies the matchmaking
-- configuration used to create the match, and contains data on all players
-- assigned to the match, including player attributes and team assignments.
-- For more details on matchmaker data, see
-- <https://docs.aws.amazon.com/gamelift/latest/flexmatchguide/match-server.html#match-server-data Match Data>.
gameSessionPlacement_matchmakerData :: Lens.Lens' GameSessionPlacement (Prelude.Maybe Prelude.Text)
gameSessionPlacement_matchmakerData = Lens.lens (\GameSessionPlacement' {matchmakerData} -> matchmakerData) (\s@GameSessionPlacement' {} a -> s {matchmakerData = a} :: GameSessionPlacement)

-- | The maximum number of players that can be connected simultaneously to
-- the game session.
gameSessionPlacement_maximumPlayerSessionCount :: Lens.Lens' GameSessionPlacement (Prelude.Maybe Prelude.Natural)
gameSessionPlacement_maximumPlayerSessionCount = Lens.lens (\GameSessionPlacement' {maximumPlayerSessionCount} -> maximumPlayerSessionCount) (\s@GameSessionPlacement' {} a -> s {maximumPlayerSessionCount = a} :: GameSessionPlacement)

-- | Time stamp indicating when this request was completed, canceled, or
-- timed out.
gameSessionPlacement_endTime :: Lens.Lens' GameSessionPlacement (Prelude.Maybe Prelude.UTCTime)
gameSessionPlacement_endTime = Lens.lens (\GameSessionPlacement' {endTime} -> endTime) (\s@GameSessionPlacement' {} a -> s {endTime = a} :: GameSessionPlacement) Prelude.. Lens.mapping Core._Time

-- | Identifier for the game session created by this placement request. This
-- value is set once the new game session is placed (placement status is
-- @FULFILLED@). This identifier is unique across all Regions. You can use
-- this value as a @GameSessionId@ value as needed.
gameSessionPlacement_gameSessionArn :: Lens.Lens' GameSessionPlacement (Prelude.Maybe Prelude.Text)
gameSessionPlacement_gameSessionArn = Lens.lens (\GameSessionPlacement' {gameSessionArn} -> gameSessionArn) (\s@GameSessionPlacement' {} a -> s {gameSessionArn = a} :: GameSessionPlacement)

-- | A set of values, expressed in milliseconds, that indicates the amount of
-- latency that a player experiences when connected to AWS Regions.
gameSessionPlacement_playerLatencies :: Lens.Lens' GameSessionPlacement (Prelude.Maybe [PlayerLatency])
gameSessionPlacement_playerLatencies = Lens.lens (\GameSessionPlacement' {playerLatencies} -> playerLatencies) (\s@GameSessionPlacement' {} a -> s {playerLatencies = a} :: GameSessionPlacement) Prelude.. Lens.mapping Lens.coerced

-- | A set of custom game session properties, formatted as a single string
-- value. This data is passed to a game server process in the GameSession
-- object with a request to start a new game session (see
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/gamelift-sdk-server-api.html#gamelift-sdk-server-startsession Start a Game Session>).
gameSessionPlacement_gameSessionData :: Lens.Lens' GameSessionPlacement (Prelude.Maybe Prelude.Text)
gameSessionPlacement_gameSessionData = Lens.lens (\GameSessionPlacement' {gameSessionData} -> gameSessionData) (\s@GameSessionPlacement' {} a -> s {gameSessionData = a} :: GameSessionPlacement)

-- | The DNS identifier assigned to the instance that is running the game
-- session. Values have the following format:
--
-- -   TLS-enabled fleets:
--     @\<unique identifier>.\<region identifier>.amazongamelift.com@.
--
-- -   Non-TLS-enabled fleets:
--     @ec2-\<unique identifier>.compute.amazonaws.com@. (See
--     <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-instance-addressing.html#concepts-public-addresses Amazon EC2 Instance IP Addressing>.)
--
-- When connecting to a game session that is running on a TLS-enabled
-- fleet, you must use the DNS name, not the IP address.
gameSessionPlacement_dnsName :: Lens.Lens' GameSessionPlacement (Prelude.Maybe Prelude.Text)
gameSessionPlacement_dnsName = Lens.lens (\GameSessionPlacement' {dnsName} -> dnsName) (\s@GameSessionPlacement' {} a -> s {dnsName = a} :: GameSessionPlacement)

-- | A descriptive label that is associated with game session queue. Queue
-- names must be unique within each Region.
gameSessionPlacement_gameSessionQueueName :: Lens.Lens' GameSessionPlacement (Prelude.Maybe Prelude.Text)
gameSessionPlacement_gameSessionQueueName = Lens.lens (\GameSessionPlacement' {gameSessionQueueName} -> gameSessionQueueName) (\s@GameSessionPlacement' {} a -> s {gameSessionQueueName = a} :: GameSessionPlacement)

-- | A collection of information on player sessions created in response to
-- the game session placement request. These player sessions are created
-- only once a new game session is successfully placed (placement status is
-- @FULFILLED@). This information includes the player ID (as provided in
-- the placement request) and the corresponding player session ID. Retrieve
-- full player sessions by calling DescribePlayerSessions with the player
-- session ID.
gameSessionPlacement_placedPlayerSessions :: Lens.Lens' GameSessionPlacement (Prelude.Maybe [PlacedPlayerSession])
gameSessionPlacement_placedPlayerSessions = Lens.lens (\GameSessionPlacement' {placedPlayerSessions} -> placedPlayerSessions) (\s@GameSessionPlacement' {} a -> s {placedPlayerSessions = a} :: GameSessionPlacement) Prelude.. Lens.mapping Lens.coerced

-- | The port number for the game session. To connect to a GameLift game
-- server, an app needs both the IP address and port number. This value is
-- set once the new game session is placed (placement status is
-- @FULFILLED@).
gameSessionPlacement_port :: Lens.Lens' GameSessionPlacement (Prelude.Maybe Prelude.Natural)
gameSessionPlacement_port = Lens.lens (\GameSessionPlacement' {port} -> port) (\s@GameSessionPlacement' {} a -> s {port = a} :: GameSessionPlacement)

instance Core.FromJSON GameSessionPlacement where
  parseJSON =
    Core.withObject
      "GameSessionPlacement"
      ( \x ->
          GameSessionPlacement'
            Prelude.<$> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "PlacementId")
            Prelude.<*> (x Core..:? "GameProperties" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "IpAddress")
            Prelude.<*> (x Core..:? "GameSessionName")
            Prelude.<*> (x Core..:? "StartTime")
            Prelude.<*> (x Core..:? "GameSessionId")
            Prelude.<*> (x Core..:? "GameSessionRegion")
            Prelude.<*> (x Core..:? "MatchmakerData")
            Prelude.<*> (x Core..:? "MaximumPlayerSessionCount")
            Prelude.<*> (x Core..:? "EndTime")
            Prelude.<*> (x Core..:? "GameSessionArn")
            Prelude.<*> ( x Core..:? "PlayerLatencies"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "GameSessionData")
            Prelude.<*> (x Core..:? "DnsName")
            Prelude.<*> (x Core..:? "GameSessionQueueName")
            Prelude.<*> ( x Core..:? "PlacedPlayerSessions"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "Port")
      )

instance Prelude.Hashable GameSessionPlacement where
  hashWithSalt salt' GameSessionPlacement' {..} =
    salt' `Prelude.hashWithSalt` port
      `Prelude.hashWithSalt` placedPlayerSessions
      `Prelude.hashWithSalt` gameSessionQueueName
      `Prelude.hashWithSalt` dnsName
      `Prelude.hashWithSalt` gameSessionData
      `Prelude.hashWithSalt` playerLatencies
      `Prelude.hashWithSalt` gameSessionArn
      `Prelude.hashWithSalt` endTime
      `Prelude.hashWithSalt` maximumPlayerSessionCount
      `Prelude.hashWithSalt` matchmakerData
      `Prelude.hashWithSalt` gameSessionRegion
      `Prelude.hashWithSalt` gameSessionId
      `Prelude.hashWithSalt` startTime
      `Prelude.hashWithSalt` gameSessionName
      `Prelude.hashWithSalt` ipAddress
      `Prelude.hashWithSalt` gameProperties
      `Prelude.hashWithSalt` placementId
      `Prelude.hashWithSalt` status

instance Prelude.NFData GameSessionPlacement where
  rnf GameSessionPlacement' {..} =
    Prelude.rnf status `Prelude.seq` Prelude.rnf port
      `Prelude.seq` Prelude.rnf placedPlayerSessions
      `Prelude.seq` Prelude.rnf gameSessionQueueName
      `Prelude.seq` Prelude.rnf dnsName
      `Prelude.seq` Prelude.rnf gameSessionData
      `Prelude.seq` Prelude.rnf playerLatencies
      `Prelude.seq` Prelude.rnf gameSessionArn
      `Prelude.seq` Prelude.rnf endTime
      `Prelude.seq` Prelude.rnf maximumPlayerSessionCount
      `Prelude.seq` Prelude.rnf matchmakerData
      `Prelude.seq` Prelude.rnf gameSessionRegion
      `Prelude.seq` Prelude.rnf gameSessionId
      `Prelude.seq` Prelude.rnf startTime
      `Prelude.seq` Prelude.rnf gameSessionName
      `Prelude.seq` Prelude.rnf ipAddress
      `Prelude.seq` Prelude.rnf gameProperties
      `Prelude.seq` Prelude.rnf placementId
