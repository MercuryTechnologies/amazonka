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
-- Module      : Amazonka.Transfer.Types.ListedServer
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Transfer.Types.ListedServer where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Transfer.Types.Domain
import Amazonka.Transfer.Types.EndpointType
import Amazonka.Transfer.Types.IdentityProviderType
import Amazonka.Transfer.Types.State

-- | Returns properties of a file transfer protocol-enabled server that was
-- specified.
--
-- /See:/ 'newListedServer' smart constructor.
data ListedServer = ListedServer'
  { -- | Specifies the Amazon Resource Name (ARN) of the Amazon Web Services
    -- Identity and Access Management (IAM) role that allows a server to turn
    -- on Amazon CloudWatch logging for Amazon S3 or Amazon EFS events. When
    -- set, user activity can be viewed in your CloudWatch logs.
    loggingRole :: Prelude.Maybe Prelude.Text,
    -- | Specifies the condition of a server for the server that was described. A
    -- value of @ONLINE@ indicates that the server can accept jobs and transfer
    -- files. A @State@ value of @OFFLINE@ means that the server cannot perform
    -- file transfer operations.
    --
    -- The states of @STARTING@ and @STOPPING@ indicate that the server is in
    -- an intermediate state, either not fully able to respond, or not fully
    -- offline. The values of @START_FAILED@ or @STOP_FAILED@ can indicate an
    -- error condition.
    state :: Prelude.Maybe State,
    -- | Specifies the mode of authentication for a server. The default value is
    -- @SERVICE_MANAGED@, which allows you to store and access user credentials
    -- within the Amazon Web Services Transfer Family service.
    --
    -- Use @AWS_DIRECTORY_SERVICE@ to provide access to Active Directory groups
    -- in Amazon Web Services Managed Active Directory or Microsoft Active
    -- Directory in your on-premises environment or in Amazon Web Services
    -- using AD Connectors. This option also requires you to provide a
    -- Directory ID using the @IdentityProviderDetails@ parameter.
    --
    -- Use the @API_GATEWAY@ value to integrate with an identity provider of
    -- your choosing. The @API_GATEWAY@ setting requires you to provide an API
    -- Gateway endpoint URL to call for authentication using the
    -- @IdentityProviderDetails@ parameter.
    identityProviderType :: Prelude.Maybe IdentityProviderType,
    -- | Specifies the unique system assigned identifier for the servers that
    -- were listed.
    serverId :: Prelude.Maybe Prelude.Text,
    -- | Specifies the domain of the storage system that is used for file
    -- transfers.
    domain :: Prelude.Maybe Domain,
    -- | Specifies the type of VPC endpoint that your server is connected to. If
    -- your server is connected to a VPC endpoint, your server isn\'t
    -- accessible over the public internet.
    endpointType :: Prelude.Maybe EndpointType,
    -- | Specifies the number of users that are assigned to a server you
    -- specified with the @ServerId@.
    userCount :: Prelude.Maybe Prelude.Int,
    -- | Specifies the unique Amazon Resource Name (ARN) for a server to be
    -- listed.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListedServer' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'loggingRole', 'listedServer_loggingRole' - Specifies the Amazon Resource Name (ARN) of the Amazon Web Services
-- Identity and Access Management (IAM) role that allows a server to turn
-- on Amazon CloudWatch logging for Amazon S3 or Amazon EFS events. When
-- set, user activity can be viewed in your CloudWatch logs.
--
-- 'state', 'listedServer_state' - Specifies the condition of a server for the server that was described. A
-- value of @ONLINE@ indicates that the server can accept jobs and transfer
-- files. A @State@ value of @OFFLINE@ means that the server cannot perform
-- file transfer operations.
--
-- The states of @STARTING@ and @STOPPING@ indicate that the server is in
-- an intermediate state, either not fully able to respond, or not fully
-- offline. The values of @START_FAILED@ or @STOP_FAILED@ can indicate an
-- error condition.
--
-- 'identityProviderType', 'listedServer_identityProviderType' - Specifies the mode of authentication for a server. The default value is
-- @SERVICE_MANAGED@, which allows you to store and access user credentials
-- within the Amazon Web Services Transfer Family service.
--
-- Use @AWS_DIRECTORY_SERVICE@ to provide access to Active Directory groups
-- in Amazon Web Services Managed Active Directory or Microsoft Active
-- Directory in your on-premises environment or in Amazon Web Services
-- using AD Connectors. This option also requires you to provide a
-- Directory ID using the @IdentityProviderDetails@ parameter.
--
-- Use the @API_GATEWAY@ value to integrate with an identity provider of
-- your choosing. The @API_GATEWAY@ setting requires you to provide an API
-- Gateway endpoint URL to call for authentication using the
-- @IdentityProviderDetails@ parameter.
--
-- 'serverId', 'listedServer_serverId' - Specifies the unique system assigned identifier for the servers that
-- were listed.
--
-- 'domain', 'listedServer_domain' - Specifies the domain of the storage system that is used for file
-- transfers.
--
-- 'endpointType', 'listedServer_endpointType' - Specifies the type of VPC endpoint that your server is connected to. If
-- your server is connected to a VPC endpoint, your server isn\'t
-- accessible over the public internet.
--
-- 'userCount', 'listedServer_userCount' - Specifies the number of users that are assigned to a server you
-- specified with the @ServerId@.
--
-- 'arn', 'listedServer_arn' - Specifies the unique Amazon Resource Name (ARN) for a server to be
-- listed.
newListedServer ::
  -- | 'arn'
  Prelude.Text ->
  ListedServer
newListedServer pArn_ =
  ListedServer'
    { loggingRole = Prelude.Nothing,
      state = Prelude.Nothing,
      identityProviderType = Prelude.Nothing,
      serverId = Prelude.Nothing,
      domain = Prelude.Nothing,
      endpointType = Prelude.Nothing,
      userCount = Prelude.Nothing,
      arn = pArn_
    }

-- | Specifies the Amazon Resource Name (ARN) of the Amazon Web Services
-- Identity and Access Management (IAM) role that allows a server to turn
-- on Amazon CloudWatch logging for Amazon S3 or Amazon EFS events. When
-- set, user activity can be viewed in your CloudWatch logs.
listedServer_loggingRole :: Lens.Lens' ListedServer (Prelude.Maybe Prelude.Text)
listedServer_loggingRole = Lens.lens (\ListedServer' {loggingRole} -> loggingRole) (\s@ListedServer' {} a -> s {loggingRole = a} :: ListedServer)

-- | Specifies the condition of a server for the server that was described. A
-- value of @ONLINE@ indicates that the server can accept jobs and transfer
-- files. A @State@ value of @OFFLINE@ means that the server cannot perform
-- file transfer operations.
--
-- The states of @STARTING@ and @STOPPING@ indicate that the server is in
-- an intermediate state, either not fully able to respond, or not fully
-- offline. The values of @START_FAILED@ or @STOP_FAILED@ can indicate an
-- error condition.
listedServer_state :: Lens.Lens' ListedServer (Prelude.Maybe State)
listedServer_state = Lens.lens (\ListedServer' {state} -> state) (\s@ListedServer' {} a -> s {state = a} :: ListedServer)

-- | Specifies the mode of authentication for a server. The default value is
-- @SERVICE_MANAGED@, which allows you to store and access user credentials
-- within the Amazon Web Services Transfer Family service.
--
-- Use @AWS_DIRECTORY_SERVICE@ to provide access to Active Directory groups
-- in Amazon Web Services Managed Active Directory or Microsoft Active
-- Directory in your on-premises environment or in Amazon Web Services
-- using AD Connectors. This option also requires you to provide a
-- Directory ID using the @IdentityProviderDetails@ parameter.
--
-- Use the @API_GATEWAY@ value to integrate with an identity provider of
-- your choosing. The @API_GATEWAY@ setting requires you to provide an API
-- Gateway endpoint URL to call for authentication using the
-- @IdentityProviderDetails@ parameter.
listedServer_identityProviderType :: Lens.Lens' ListedServer (Prelude.Maybe IdentityProviderType)
listedServer_identityProviderType = Lens.lens (\ListedServer' {identityProviderType} -> identityProviderType) (\s@ListedServer' {} a -> s {identityProviderType = a} :: ListedServer)

-- | Specifies the unique system assigned identifier for the servers that
-- were listed.
listedServer_serverId :: Lens.Lens' ListedServer (Prelude.Maybe Prelude.Text)
listedServer_serverId = Lens.lens (\ListedServer' {serverId} -> serverId) (\s@ListedServer' {} a -> s {serverId = a} :: ListedServer)

-- | Specifies the domain of the storage system that is used for file
-- transfers.
listedServer_domain :: Lens.Lens' ListedServer (Prelude.Maybe Domain)
listedServer_domain = Lens.lens (\ListedServer' {domain} -> domain) (\s@ListedServer' {} a -> s {domain = a} :: ListedServer)

-- | Specifies the type of VPC endpoint that your server is connected to. If
-- your server is connected to a VPC endpoint, your server isn\'t
-- accessible over the public internet.
listedServer_endpointType :: Lens.Lens' ListedServer (Prelude.Maybe EndpointType)
listedServer_endpointType = Lens.lens (\ListedServer' {endpointType} -> endpointType) (\s@ListedServer' {} a -> s {endpointType = a} :: ListedServer)

-- | Specifies the number of users that are assigned to a server you
-- specified with the @ServerId@.
listedServer_userCount :: Lens.Lens' ListedServer (Prelude.Maybe Prelude.Int)
listedServer_userCount = Lens.lens (\ListedServer' {userCount} -> userCount) (\s@ListedServer' {} a -> s {userCount = a} :: ListedServer)

-- | Specifies the unique Amazon Resource Name (ARN) for a server to be
-- listed.
listedServer_arn :: Lens.Lens' ListedServer Prelude.Text
listedServer_arn = Lens.lens (\ListedServer' {arn} -> arn) (\s@ListedServer' {} a -> s {arn = a} :: ListedServer)

instance Core.FromJSON ListedServer where
  parseJSON =
    Core.withObject
      "ListedServer"
      ( \x ->
          ListedServer'
            Prelude.<$> (x Core..:? "LoggingRole")
            Prelude.<*> (x Core..:? "State")
            Prelude.<*> (x Core..:? "IdentityProviderType")
            Prelude.<*> (x Core..:? "ServerId")
            Prelude.<*> (x Core..:? "Domain")
            Prelude.<*> (x Core..:? "EndpointType")
            Prelude.<*> (x Core..:? "UserCount")
            Prelude.<*> (x Core..: "Arn")
      )

instance Prelude.Hashable ListedServer where
  hashWithSalt salt' ListedServer' {..} =
    salt' `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` userCount
      `Prelude.hashWithSalt` endpointType
      `Prelude.hashWithSalt` domain
      `Prelude.hashWithSalt` serverId
      `Prelude.hashWithSalt` identityProviderType
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` loggingRole

instance Prelude.NFData ListedServer where
  rnf ListedServer' {..} =
    Prelude.rnf loggingRole
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf userCount
      `Prelude.seq` Prelude.rnf endpointType
      `Prelude.seq` Prelude.rnf domain
      `Prelude.seq` Prelude.rnf serverId
      `Prelude.seq` Prelude.rnf identityProviderType
      `Prelude.seq` Prelude.rnf state
