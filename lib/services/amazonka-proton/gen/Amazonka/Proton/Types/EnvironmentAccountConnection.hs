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
-- Module      : Amazonka.Proton.Types.EnvironmentAccountConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Proton.Types.EnvironmentAccountConnection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.Proton.Types.EnvironmentAccountConnectionStatus

-- | The environment account connection detail data.
--
-- /See:/ 'newEnvironmentAccountConnection' smart constructor.
data EnvironmentAccountConnection = EnvironmentAccountConnection'
  { -- | The Amazon Resource Name (ARN) of the environment account connection.
    arn :: Prelude.Text,
    -- | The environment account that\'s connected to the environment account
    -- connection.
    environmentAccountId :: Prelude.Text,
    -- | The name of the environment that\'s associated with the environment
    -- account connection.
    environmentName :: Prelude.Text,
    -- | The ID of the environment account connection.
    id :: Prelude.Text,
    -- | The time when the environment account connection was last modified.
    lastModifiedAt :: Core.POSIX,
    -- | The ID of the management account that\'s connected to the environment
    -- account connection.
    managementAccountId :: Prelude.Text,
    -- | The time when the environment account connection request was made.
    requestedAt :: Core.POSIX,
    -- | The IAM service role that\'s associated with the environment account
    -- connection.
    roleArn :: Prelude.Text,
    -- | The status of the environment account connection.
    status :: EnvironmentAccountConnectionStatus
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'EnvironmentAccountConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'environmentAccountConnection_arn' - The Amazon Resource Name (ARN) of the environment account connection.
--
-- 'environmentAccountId', 'environmentAccountConnection_environmentAccountId' - The environment account that\'s connected to the environment account
-- connection.
--
-- 'environmentName', 'environmentAccountConnection_environmentName' - The name of the environment that\'s associated with the environment
-- account connection.
--
-- 'id', 'environmentAccountConnection_id' - The ID of the environment account connection.
--
-- 'lastModifiedAt', 'environmentAccountConnection_lastModifiedAt' - The time when the environment account connection was last modified.
--
-- 'managementAccountId', 'environmentAccountConnection_managementAccountId' - The ID of the management account that\'s connected to the environment
-- account connection.
--
-- 'requestedAt', 'environmentAccountConnection_requestedAt' - The time when the environment account connection request was made.
--
-- 'roleArn', 'environmentAccountConnection_roleArn' - The IAM service role that\'s associated with the environment account
-- connection.
--
-- 'status', 'environmentAccountConnection_status' - The status of the environment account connection.
newEnvironmentAccountConnection ::
  -- | 'arn'
  Prelude.Text ->
  -- | 'environmentAccountId'
  Prelude.Text ->
  -- | 'environmentName'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'lastModifiedAt'
  Prelude.UTCTime ->
  -- | 'managementAccountId'
  Prelude.Text ->
  -- | 'requestedAt'
  Prelude.UTCTime ->
  -- | 'roleArn'
  Prelude.Text ->
  -- | 'status'
  EnvironmentAccountConnectionStatus ->
  EnvironmentAccountConnection
newEnvironmentAccountConnection
  pArn_
  pEnvironmentAccountId_
  pEnvironmentName_
  pId_
  pLastModifiedAt_
  pManagementAccountId_
  pRequestedAt_
  pRoleArn_
  pStatus_ =
    EnvironmentAccountConnection'
      { arn = pArn_,
        environmentAccountId = pEnvironmentAccountId_,
        environmentName = pEnvironmentName_,
        id = pId_,
        lastModifiedAt =
          Core._Time Lens.# pLastModifiedAt_,
        managementAccountId = pManagementAccountId_,
        requestedAt = Core._Time Lens.# pRequestedAt_,
        roleArn = pRoleArn_,
        status = pStatus_
      }

-- | The Amazon Resource Name (ARN) of the environment account connection.
environmentAccountConnection_arn :: Lens.Lens' EnvironmentAccountConnection Prelude.Text
environmentAccountConnection_arn = Lens.lens (\EnvironmentAccountConnection' {arn} -> arn) (\s@EnvironmentAccountConnection' {} a -> s {arn = a} :: EnvironmentAccountConnection)

-- | The environment account that\'s connected to the environment account
-- connection.
environmentAccountConnection_environmentAccountId :: Lens.Lens' EnvironmentAccountConnection Prelude.Text
environmentAccountConnection_environmentAccountId = Lens.lens (\EnvironmentAccountConnection' {environmentAccountId} -> environmentAccountId) (\s@EnvironmentAccountConnection' {} a -> s {environmentAccountId = a} :: EnvironmentAccountConnection)

-- | The name of the environment that\'s associated with the environment
-- account connection.
environmentAccountConnection_environmentName :: Lens.Lens' EnvironmentAccountConnection Prelude.Text
environmentAccountConnection_environmentName = Lens.lens (\EnvironmentAccountConnection' {environmentName} -> environmentName) (\s@EnvironmentAccountConnection' {} a -> s {environmentName = a} :: EnvironmentAccountConnection)

-- | The ID of the environment account connection.
environmentAccountConnection_id :: Lens.Lens' EnvironmentAccountConnection Prelude.Text
environmentAccountConnection_id = Lens.lens (\EnvironmentAccountConnection' {id} -> id) (\s@EnvironmentAccountConnection' {} a -> s {id = a} :: EnvironmentAccountConnection)

-- | The time when the environment account connection was last modified.
environmentAccountConnection_lastModifiedAt :: Lens.Lens' EnvironmentAccountConnection Prelude.UTCTime
environmentAccountConnection_lastModifiedAt = Lens.lens (\EnvironmentAccountConnection' {lastModifiedAt} -> lastModifiedAt) (\s@EnvironmentAccountConnection' {} a -> s {lastModifiedAt = a} :: EnvironmentAccountConnection) Prelude.. Core._Time

-- | The ID of the management account that\'s connected to the environment
-- account connection.
environmentAccountConnection_managementAccountId :: Lens.Lens' EnvironmentAccountConnection Prelude.Text
environmentAccountConnection_managementAccountId = Lens.lens (\EnvironmentAccountConnection' {managementAccountId} -> managementAccountId) (\s@EnvironmentAccountConnection' {} a -> s {managementAccountId = a} :: EnvironmentAccountConnection)

-- | The time when the environment account connection request was made.
environmentAccountConnection_requestedAt :: Lens.Lens' EnvironmentAccountConnection Prelude.UTCTime
environmentAccountConnection_requestedAt = Lens.lens (\EnvironmentAccountConnection' {requestedAt} -> requestedAt) (\s@EnvironmentAccountConnection' {} a -> s {requestedAt = a} :: EnvironmentAccountConnection) Prelude.. Core._Time

-- | The IAM service role that\'s associated with the environment account
-- connection.
environmentAccountConnection_roleArn :: Lens.Lens' EnvironmentAccountConnection Prelude.Text
environmentAccountConnection_roleArn = Lens.lens (\EnvironmentAccountConnection' {roleArn} -> roleArn) (\s@EnvironmentAccountConnection' {} a -> s {roleArn = a} :: EnvironmentAccountConnection)

-- | The status of the environment account connection.
environmentAccountConnection_status :: Lens.Lens' EnvironmentAccountConnection EnvironmentAccountConnectionStatus
environmentAccountConnection_status = Lens.lens (\EnvironmentAccountConnection' {status} -> status) (\s@EnvironmentAccountConnection' {} a -> s {status = a} :: EnvironmentAccountConnection)

instance Core.FromJSON EnvironmentAccountConnection where
  parseJSON =
    Core.withObject
      "EnvironmentAccountConnection"
      ( \x ->
          EnvironmentAccountConnection'
            Prelude.<$> (x Core..: "arn")
            Prelude.<*> (x Core..: "environmentAccountId")
            Prelude.<*> (x Core..: "environmentName")
            Prelude.<*> (x Core..: "id")
            Prelude.<*> (x Core..: "lastModifiedAt")
            Prelude.<*> (x Core..: "managementAccountId")
            Prelude.<*> (x Core..: "requestedAt")
            Prelude.<*> (x Core..: "roleArn")
            Prelude.<*> (x Core..: "status")
      )

instance
  Prelude.Hashable
    EnvironmentAccountConnection
  where
  hashWithSalt salt' EnvironmentAccountConnection' {..} =
    salt' `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` roleArn
      `Prelude.hashWithSalt` requestedAt
      `Prelude.hashWithSalt` managementAccountId
      `Prelude.hashWithSalt` lastModifiedAt
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` environmentName
      `Prelude.hashWithSalt` environmentAccountId
      `Prelude.hashWithSalt` arn

instance Prelude.NFData EnvironmentAccountConnection where
  rnf EnvironmentAccountConnection' {..} =
    Prelude.rnf arn `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf roleArn
      `Prelude.seq` Prelude.rnf requestedAt
      `Prelude.seq` Prelude.rnf managementAccountId
      `Prelude.seq` Prelude.rnf lastModifiedAt
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf environmentName
      `Prelude.seq` Prelude.rnf environmentAccountId
