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
-- Module      : Amazonka.Cloud9.Types.Environment
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Cloud9.Types.Environment where

import Amazonka.Cloud9.Types.ConnectionType
import Amazonka.Cloud9.Types.EnvironmentLifecycle
import Amazonka.Cloud9.Types.EnvironmentType
import Amazonka.Cloud9.Types.ManagedCredentialsStatus
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Information about an Cloud9 development environment.
--
-- /See:/ 'newEnvironment' smart constructor.
data Environment = Environment'
  { -- | The state of the environment in its creation or deletion lifecycle.
    lifecycle :: Prelude.Maybe EnvironmentLifecycle,
    -- | The name of the environment.
    name :: Prelude.Maybe Prelude.Text,
    -- | The ID of the environment.
    id :: Prelude.Maybe Prelude.Text,
    -- | The connection type used for connecting to an Amazon EC2 environment.
    -- @CONNECT_SSH@ is selected by default.
    connectionType :: Prelude.Maybe ConnectionType,
    -- | The description for the environment.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | Describes the status of Amazon Web Services managed temporary
    -- credentials for the Cloud9 environment. Available values are:
    --
    -- -   @ENABLED_ON_CREATE@
    --
    -- -   @ENABLED_BY_OWNER@
    --
    -- -   @DISABLED_BY_DEFAULT@
    --
    -- -   @DISABLED_BY_OWNER@
    --
    -- -   @DISABLED_BY_COLLABORATOR@
    --
    -- -   @PENDING_REMOVAL_BY_COLLABORATOR@
    --
    -- -   @PENDING_REMOVAL_BY_OWNER@
    --
    -- -   @FAILED_REMOVAL_BY_COLLABORATOR@
    --
    -- -   @ENABLED_BY_OWNER@
    --
    -- -   @DISABLED_BY_DEFAULT@
    managedCredentialsStatus :: Prelude.Maybe ManagedCredentialsStatus,
    -- | The type of environment. Valid values include the following:
    --
    -- -   @ec2@: An Amazon Elastic Compute Cloud (Amazon EC2) instance
    --     connects to the environment.
    --
    -- -   @ssh@: Your own server connects to the environment.
    type' :: EnvironmentType,
    -- | The Amazon Resource Name (ARN) of the environment.
    arn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the environment owner.
    ownerArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Environment' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lifecycle', 'environment_lifecycle' - The state of the environment in its creation or deletion lifecycle.
--
-- 'name', 'environment_name' - The name of the environment.
--
-- 'id', 'environment_id' - The ID of the environment.
--
-- 'connectionType', 'environment_connectionType' - The connection type used for connecting to an Amazon EC2 environment.
-- @CONNECT_SSH@ is selected by default.
--
-- 'description', 'environment_description' - The description for the environment.
--
-- 'managedCredentialsStatus', 'environment_managedCredentialsStatus' - Describes the status of Amazon Web Services managed temporary
-- credentials for the Cloud9 environment. Available values are:
--
-- -   @ENABLED_ON_CREATE@
--
-- -   @ENABLED_BY_OWNER@
--
-- -   @DISABLED_BY_DEFAULT@
--
-- -   @DISABLED_BY_OWNER@
--
-- -   @DISABLED_BY_COLLABORATOR@
--
-- -   @PENDING_REMOVAL_BY_COLLABORATOR@
--
-- -   @PENDING_REMOVAL_BY_OWNER@
--
-- -   @FAILED_REMOVAL_BY_COLLABORATOR@
--
-- -   @ENABLED_BY_OWNER@
--
-- -   @DISABLED_BY_DEFAULT@
--
-- 'type'', 'environment_type' - The type of environment. Valid values include the following:
--
-- -   @ec2@: An Amazon Elastic Compute Cloud (Amazon EC2) instance
--     connects to the environment.
--
-- -   @ssh@: Your own server connects to the environment.
--
-- 'arn', 'environment_arn' - The Amazon Resource Name (ARN) of the environment.
--
-- 'ownerArn', 'environment_ownerArn' - The Amazon Resource Name (ARN) of the environment owner.
newEnvironment ::
  -- | 'type''
  EnvironmentType ->
  -- | 'arn'
  Prelude.Text ->
  -- | 'ownerArn'
  Prelude.Text ->
  Environment
newEnvironment pType_ pArn_ pOwnerArn_ =
  Environment'
    { lifecycle = Prelude.Nothing,
      name = Prelude.Nothing,
      id = Prelude.Nothing,
      connectionType = Prelude.Nothing,
      description = Prelude.Nothing,
      managedCredentialsStatus = Prelude.Nothing,
      type' = pType_,
      arn = pArn_,
      ownerArn = pOwnerArn_
    }

-- | The state of the environment in its creation or deletion lifecycle.
environment_lifecycle :: Lens.Lens' Environment (Prelude.Maybe EnvironmentLifecycle)
environment_lifecycle = Lens.lens (\Environment' {lifecycle} -> lifecycle) (\s@Environment' {} a -> s {lifecycle = a} :: Environment)

-- | The name of the environment.
environment_name :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_name = Lens.lens (\Environment' {name} -> name) (\s@Environment' {} a -> s {name = a} :: Environment)

-- | The ID of the environment.
environment_id :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_id = Lens.lens (\Environment' {id} -> id) (\s@Environment' {} a -> s {id = a} :: Environment)

-- | The connection type used for connecting to an Amazon EC2 environment.
-- @CONNECT_SSH@ is selected by default.
environment_connectionType :: Lens.Lens' Environment (Prelude.Maybe ConnectionType)
environment_connectionType = Lens.lens (\Environment' {connectionType} -> connectionType) (\s@Environment' {} a -> s {connectionType = a} :: Environment)

-- | The description for the environment.
environment_description :: Lens.Lens' Environment (Prelude.Maybe Prelude.Text)
environment_description = Lens.lens (\Environment' {description} -> description) (\s@Environment' {} a -> s {description = a} :: Environment) Prelude.. Lens.mapping Core._Sensitive

-- | Describes the status of Amazon Web Services managed temporary
-- credentials for the Cloud9 environment. Available values are:
--
-- -   @ENABLED_ON_CREATE@
--
-- -   @ENABLED_BY_OWNER@
--
-- -   @DISABLED_BY_DEFAULT@
--
-- -   @DISABLED_BY_OWNER@
--
-- -   @DISABLED_BY_COLLABORATOR@
--
-- -   @PENDING_REMOVAL_BY_COLLABORATOR@
--
-- -   @PENDING_REMOVAL_BY_OWNER@
--
-- -   @FAILED_REMOVAL_BY_COLLABORATOR@
--
-- -   @ENABLED_BY_OWNER@
--
-- -   @DISABLED_BY_DEFAULT@
environment_managedCredentialsStatus :: Lens.Lens' Environment (Prelude.Maybe ManagedCredentialsStatus)
environment_managedCredentialsStatus = Lens.lens (\Environment' {managedCredentialsStatus} -> managedCredentialsStatus) (\s@Environment' {} a -> s {managedCredentialsStatus = a} :: Environment)

-- | The type of environment. Valid values include the following:
--
-- -   @ec2@: An Amazon Elastic Compute Cloud (Amazon EC2) instance
--     connects to the environment.
--
-- -   @ssh@: Your own server connects to the environment.
environment_type :: Lens.Lens' Environment EnvironmentType
environment_type = Lens.lens (\Environment' {type'} -> type') (\s@Environment' {} a -> s {type' = a} :: Environment)

-- | The Amazon Resource Name (ARN) of the environment.
environment_arn :: Lens.Lens' Environment Prelude.Text
environment_arn = Lens.lens (\Environment' {arn} -> arn) (\s@Environment' {} a -> s {arn = a} :: Environment)

-- | The Amazon Resource Name (ARN) of the environment owner.
environment_ownerArn :: Lens.Lens' Environment Prelude.Text
environment_ownerArn = Lens.lens (\Environment' {ownerArn} -> ownerArn) (\s@Environment' {} a -> s {ownerArn = a} :: Environment)

instance Core.FromJSON Environment where
  parseJSON =
    Core.withObject
      "Environment"
      ( \x ->
          Environment'
            Prelude.<$> (x Core..:? "lifecycle")
            Prelude.<*> (x Core..:? "name")
            Prelude.<*> (x Core..:? "id")
            Prelude.<*> (x Core..:? "connectionType")
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..:? "managedCredentialsStatus")
            Prelude.<*> (x Core..: "type")
            Prelude.<*> (x Core..: "arn")
            Prelude.<*> (x Core..: "ownerArn")
      )

instance Prelude.Hashable Environment where
  hashWithSalt salt' Environment' {..} =
    salt' `Prelude.hashWithSalt` ownerArn
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` type'
      `Prelude.hashWithSalt` managedCredentialsStatus
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` connectionType
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` lifecycle

instance Prelude.NFData Environment where
  rnf Environment' {..} =
    Prelude.rnf lifecycle
      `Prelude.seq` Prelude.rnf ownerArn
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf type'
      `Prelude.seq` Prelude.rnf managedCredentialsStatus
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf connectionType
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf name
