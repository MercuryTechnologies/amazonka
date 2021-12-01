{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.ConnectParticipant.CreateParticipantConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates the participant\'s connection. Note that ParticipantToken is
-- used for invoking this API instead of ConnectionToken.
--
-- The participant token is valid for the lifetime of the participant –
-- until they are part of a contact.
--
-- The response URL for @WEBSOCKET@ Type has a connect expiry timeout of
-- 100s. Clients must manually connect to the returned websocket URL and
-- subscribe to the desired topic.
--
-- For chat, you need to publish the following on the established websocket
-- connection:
--
-- @{\"topic\":\"aws\/subscribe\",\"content\":{\"topics\":[\"aws\/chat\"]}}@
--
-- Upon websocket URL expiry, as specified in the response ConnectionExpiry
-- parameter, clients need to call this API again to obtain a new websocket
-- URL and perform the same steps as before.
--
-- The Amazon Connect Participant Service APIs do not use
-- <https://docs.aws.amazon.com/general/latest/gr/signature-version-4.html Signature Version 4 authentication>.
module Amazonka.ConnectParticipant.CreateParticipantConnection
  ( -- * Creating a Request
    CreateParticipantConnection (..),
    newCreateParticipantConnection,

    -- * Request Lenses
    createParticipantConnection_type,
    createParticipantConnection_participantToken,

    -- * Destructuring the Response
    CreateParticipantConnectionResponse (..),
    newCreateParticipantConnectionResponse,

    -- * Response Lenses
    createParticipantConnectionResponse_connectionCredentials,
    createParticipantConnectionResponse_websocket,
    createParticipantConnectionResponse_httpStatus,
  )
where

import Amazonka.ConnectParticipant.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateParticipantConnection' smart constructor.
data CreateParticipantConnection = CreateParticipantConnection'
  { -- | Type of connection information required.
    type' :: Prelude.NonEmpty ConnectionType,
    -- | This is a header parameter.
    --
    -- The Participant Token as obtained from
    -- <https://docs.aws.amazon.com/connect/latest/APIReference/API_StartChatContact.html StartChatContact>
    -- API response.
    participantToken :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateParticipantConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'type'', 'createParticipantConnection_type' - Type of connection information required.
--
-- 'participantToken', 'createParticipantConnection_participantToken' - This is a header parameter.
--
-- The Participant Token as obtained from
-- <https://docs.aws.amazon.com/connect/latest/APIReference/API_StartChatContact.html StartChatContact>
-- API response.
newCreateParticipantConnection ::
  -- | 'type''
  Prelude.NonEmpty ConnectionType ->
  -- | 'participantToken'
  Prelude.Text ->
  CreateParticipantConnection
newCreateParticipantConnection
  pType_
  pParticipantToken_ =
    CreateParticipantConnection'
      { type' =
          Lens.coerced Lens.# pType_,
        participantToken = pParticipantToken_
      }

-- | Type of connection information required.
createParticipantConnection_type :: Lens.Lens' CreateParticipantConnection (Prelude.NonEmpty ConnectionType)
createParticipantConnection_type = Lens.lens (\CreateParticipantConnection' {type'} -> type') (\s@CreateParticipantConnection' {} a -> s {type' = a} :: CreateParticipantConnection) Prelude.. Lens.coerced

-- | This is a header parameter.
--
-- The Participant Token as obtained from
-- <https://docs.aws.amazon.com/connect/latest/APIReference/API_StartChatContact.html StartChatContact>
-- API response.
createParticipantConnection_participantToken :: Lens.Lens' CreateParticipantConnection Prelude.Text
createParticipantConnection_participantToken = Lens.lens (\CreateParticipantConnection' {participantToken} -> participantToken) (\s@CreateParticipantConnection' {} a -> s {participantToken = a} :: CreateParticipantConnection)

instance Core.AWSRequest CreateParticipantConnection where
  type
    AWSResponse CreateParticipantConnection =
      CreateParticipantConnectionResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateParticipantConnectionResponse'
            Prelude.<$> (x Core..?> "ConnectionCredentials")
            Prelude.<*> (x Core..?> "Websocket")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateParticipantConnection where
  hashWithSalt salt' CreateParticipantConnection' {..} =
    salt' `Prelude.hashWithSalt` participantToken
      `Prelude.hashWithSalt` type'

instance Prelude.NFData CreateParticipantConnection where
  rnf CreateParticipantConnection' {..} =
    Prelude.rnf type'
      `Prelude.seq` Prelude.rnf participantToken

instance Core.ToHeaders CreateParticipantConnection where
  toHeaders CreateParticipantConnection' {..} =
    Prelude.mconcat
      [ "X-Amz-Bearer" Core.=# participantToken,
        "Content-Type"
          Core.=# ("application/x-amz-json-1.1" :: Prelude.ByteString)
      ]

instance Core.ToJSON CreateParticipantConnection where
  toJSON CreateParticipantConnection' {..} =
    Core.object
      ( Prelude.catMaybes
          [Prelude.Just ("Type" Core..= type')]
      )

instance Core.ToPath CreateParticipantConnection where
  toPath = Prelude.const "/participant/connection"

instance Core.ToQuery CreateParticipantConnection where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateParticipantConnectionResponse' smart constructor.
data CreateParticipantConnectionResponse = CreateParticipantConnectionResponse'
  { -- | Creates the participant\'s connection credentials. The authentication
    -- token associated with the participant\'s connection.
    connectionCredentials :: Prelude.Maybe ConnectionCredentials,
    -- | Creates the participant\'s websocket connection.
    websocket :: Prelude.Maybe Websocket,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateParticipantConnectionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionCredentials', 'createParticipantConnectionResponse_connectionCredentials' - Creates the participant\'s connection credentials. The authentication
-- token associated with the participant\'s connection.
--
-- 'websocket', 'createParticipantConnectionResponse_websocket' - Creates the participant\'s websocket connection.
--
-- 'httpStatus', 'createParticipantConnectionResponse_httpStatus' - The response's http status code.
newCreateParticipantConnectionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateParticipantConnectionResponse
newCreateParticipantConnectionResponse pHttpStatus_ =
  CreateParticipantConnectionResponse'
    { connectionCredentials =
        Prelude.Nothing,
      websocket = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Creates the participant\'s connection credentials. The authentication
-- token associated with the participant\'s connection.
createParticipantConnectionResponse_connectionCredentials :: Lens.Lens' CreateParticipantConnectionResponse (Prelude.Maybe ConnectionCredentials)
createParticipantConnectionResponse_connectionCredentials = Lens.lens (\CreateParticipantConnectionResponse' {connectionCredentials} -> connectionCredentials) (\s@CreateParticipantConnectionResponse' {} a -> s {connectionCredentials = a} :: CreateParticipantConnectionResponse)

-- | Creates the participant\'s websocket connection.
createParticipantConnectionResponse_websocket :: Lens.Lens' CreateParticipantConnectionResponse (Prelude.Maybe Websocket)
createParticipantConnectionResponse_websocket = Lens.lens (\CreateParticipantConnectionResponse' {websocket} -> websocket) (\s@CreateParticipantConnectionResponse' {} a -> s {websocket = a} :: CreateParticipantConnectionResponse)

-- | The response's http status code.
createParticipantConnectionResponse_httpStatus :: Lens.Lens' CreateParticipantConnectionResponse Prelude.Int
createParticipantConnectionResponse_httpStatus = Lens.lens (\CreateParticipantConnectionResponse' {httpStatus} -> httpStatus) (\s@CreateParticipantConnectionResponse' {} a -> s {httpStatus = a} :: CreateParticipantConnectionResponse)

instance
  Prelude.NFData
    CreateParticipantConnectionResponse
  where
  rnf CreateParticipantConnectionResponse' {..} =
    Prelude.rnf connectionCredentials
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf websocket
