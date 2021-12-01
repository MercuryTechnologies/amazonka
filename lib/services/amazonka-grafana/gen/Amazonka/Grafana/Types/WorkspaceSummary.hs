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
-- Module      : Amazonka.Grafana.Types.WorkspaceSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Grafana.Types.WorkspaceSummary where

import qualified Amazonka.Core as Core
import Amazonka.Grafana.Types.AuthenticationSummary
import Amazonka.Grafana.Types.NotificationDestinationType
import Amazonka.Grafana.Types.WorkspaceStatus
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | A structure that contains some information about one workspace in the
-- account.
--
-- /See:/ 'newWorkspaceSummary' smart constructor.
data WorkspaceSummary = WorkspaceSummary'
  { -- | The name of the workspace.
    name :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The Amazon Web Services notification channels that Amazon Managed
    -- Grafana can automatically create IAM roles and permissions for, which
    -- allows Amazon Managed Grafana to use these channels.
    notificationDestinations :: Prelude.Maybe [NotificationDestinationType],
    -- | The customer-entered description of the workspace.
    description :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | A structure containing information about the authentication methods used
    -- in the workspace.
    authentication :: AuthenticationSummary,
    -- | The date that the workspace was created.
    created :: Core.POSIX,
    -- | The URL endpoint to use to access the Grafana console in the workspace.
    endpoint :: Prelude.Text,
    -- | The Grafana version that the workspace is running.
    grafanaVersion :: Prelude.Text,
    -- | The unique ID of the workspace.
    id :: Prelude.Text,
    -- | The most recent date that the workspace was modified.
    modified :: Core.POSIX,
    -- | The current status of the workspace.
    status :: WorkspaceStatus
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'WorkspaceSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'workspaceSummary_name' - The name of the workspace.
--
-- 'notificationDestinations', 'workspaceSummary_notificationDestinations' - The Amazon Web Services notification channels that Amazon Managed
-- Grafana can automatically create IAM roles and permissions for, which
-- allows Amazon Managed Grafana to use these channels.
--
-- 'description', 'workspaceSummary_description' - The customer-entered description of the workspace.
--
-- 'authentication', 'workspaceSummary_authentication' - A structure containing information about the authentication methods used
-- in the workspace.
--
-- 'created', 'workspaceSummary_created' - The date that the workspace was created.
--
-- 'endpoint', 'workspaceSummary_endpoint' - The URL endpoint to use to access the Grafana console in the workspace.
--
-- 'grafanaVersion', 'workspaceSummary_grafanaVersion' - The Grafana version that the workspace is running.
--
-- 'id', 'workspaceSummary_id' - The unique ID of the workspace.
--
-- 'modified', 'workspaceSummary_modified' - The most recent date that the workspace was modified.
--
-- 'status', 'workspaceSummary_status' - The current status of the workspace.
newWorkspaceSummary ::
  -- | 'authentication'
  AuthenticationSummary ->
  -- | 'created'
  Prelude.UTCTime ->
  -- | 'endpoint'
  Prelude.Text ->
  -- | 'grafanaVersion'
  Prelude.Text ->
  -- | 'id'
  Prelude.Text ->
  -- | 'modified'
  Prelude.UTCTime ->
  -- | 'status'
  WorkspaceStatus ->
  WorkspaceSummary
newWorkspaceSummary
  pAuthentication_
  pCreated_
  pEndpoint_
  pGrafanaVersion_
  pId_
  pModified_
  pStatus_ =
    WorkspaceSummary'
      { name = Prelude.Nothing,
        notificationDestinations = Prelude.Nothing,
        description = Prelude.Nothing,
        authentication = pAuthentication_,
        created = Core._Time Lens.# pCreated_,
        endpoint = pEndpoint_,
        grafanaVersion = pGrafanaVersion_,
        id = pId_,
        modified = Core._Time Lens.# pModified_,
        status = pStatus_
      }

-- | The name of the workspace.
workspaceSummary_name :: Lens.Lens' WorkspaceSummary (Prelude.Maybe Prelude.Text)
workspaceSummary_name = Lens.lens (\WorkspaceSummary' {name} -> name) (\s@WorkspaceSummary' {} a -> s {name = a} :: WorkspaceSummary) Prelude.. Lens.mapping Core._Sensitive

-- | The Amazon Web Services notification channels that Amazon Managed
-- Grafana can automatically create IAM roles and permissions for, which
-- allows Amazon Managed Grafana to use these channels.
workspaceSummary_notificationDestinations :: Lens.Lens' WorkspaceSummary (Prelude.Maybe [NotificationDestinationType])
workspaceSummary_notificationDestinations = Lens.lens (\WorkspaceSummary' {notificationDestinations} -> notificationDestinations) (\s@WorkspaceSummary' {} a -> s {notificationDestinations = a} :: WorkspaceSummary) Prelude.. Lens.mapping Lens.coerced

-- | The customer-entered description of the workspace.
workspaceSummary_description :: Lens.Lens' WorkspaceSummary (Prelude.Maybe Prelude.Text)
workspaceSummary_description = Lens.lens (\WorkspaceSummary' {description} -> description) (\s@WorkspaceSummary' {} a -> s {description = a} :: WorkspaceSummary) Prelude.. Lens.mapping Core._Sensitive

-- | A structure containing information about the authentication methods used
-- in the workspace.
workspaceSummary_authentication :: Lens.Lens' WorkspaceSummary AuthenticationSummary
workspaceSummary_authentication = Lens.lens (\WorkspaceSummary' {authentication} -> authentication) (\s@WorkspaceSummary' {} a -> s {authentication = a} :: WorkspaceSummary)

-- | The date that the workspace was created.
workspaceSummary_created :: Lens.Lens' WorkspaceSummary Prelude.UTCTime
workspaceSummary_created = Lens.lens (\WorkspaceSummary' {created} -> created) (\s@WorkspaceSummary' {} a -> s {created = a} :: WorkspaceSummary) Prelude.. Core._Time

-- | The URL endpoint to use to access the Grafana console in the workspace.
workspaceSummary_endpoint :: Lens.Lens' WorkspaceSummary Prelude.Text
workspaceSummary_endpoint = Lens.lens (\WorkspaceSummary' {endpoint} -> endpoint) (\s@WorkspaceSummary' {} a -> s {endpoint = a} :: WorkspaceSummary)

-- | The Grafana version that the workspace is running.
workspaceSummary_grafanaVersion :: Lens.Lens' WorkspaceSummary Prelude.Text
workspaceSummary_grafanaVersion = Lens.lens (\WorkspaceSummary' {grafanaVersion} -> grafanaVersion) (\s@WorkspaceSummary' {} a -> s {grafanaVersion = a} :: WorkspaceSummary)

-- | The unique ID of the workspace.
workspaceSummary_id :: Lens.Lens' WorkspaceSummary Prelude.Text
workspaceSummary_id = Lens.lens (\WorkspaceSummary' {id} -> id) (\s@WorkspaceSummary' {} a -> s {id = a} :: WorkspaceSummary)

-- | The most recent date that the workspace was modified.
workspaceSummary_modified :: Lens.Lens' WorkspaceSummary Prelude.UTCTime
workspaceSummary_modified = Lens.lens (\WorkspaceSummary' {modified} -> modified) (\s@WorkspaceSummary' {} a -> s {modified = a} :: WorkspaceSummary) Prelude.. Core._Time

-- | The current status of the workspace.
workspaceSummary_status :: Lens.Lens' WorkspaceSummary WorkspaceStatus
workspaceSummary_status = Lens.lens (\WorkspaceSummary' {status} -> status) (\s@WorkspaceSummary' {} a -> s {status = a} :: WorkspaceSummary)

instance Core.FromJSON WorkspaceSummary where
  parseJSON =
    Core.withObject
      "WorkspaceSummary"
      ( \x ->
          WorkspaceSummary'
            Prelude.<$> (x Core..:? "name")
            Prelude.<*> ( x Core..:? "notificationDestinations"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "description")
            Prelude.<*> (x Core..: "authentication")
            Prelude.<*> (x Core..: "created")
            Prelude.<*> (x Core..: "endpoint")
            Prelude.<*> (x Core..: "grafanaVersion")
            Prelude.<*> (x Core..: "id")
            Prelude.<*> (x Core..: "modified")
            Prelude.<*> (x Core..: "status")
      )

instance Prelude.Hashable WorkspaceSummary where
  hashWithSalt salt' WorkspaceSummary' {..} =
    salt' `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` modified
      `Prelude.hashWithSalt` id
      `Prelude.hashWithSalt` grafanaVersion
      `Prelude.hashWithSalt` endpoint
      `Prelude.hashWithSalt` created
      `Prelude.hashWithSalt` authentication
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` notificationDestinations
      `Prelude.hashWithSalt` name

instance Prelude.NFData WorkspaceSummary where
  rnf WorkspaceSummary' {..} =
    Prelude.rnf name `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf modified
      `Prelude.seq` Prelude.rnf id
      `Prelude.seq` Prelude.rnf grafanaVersion
      `Prelude.seq` Prelude.rnf endpoint
      `Prelude.seq` Prelude.rnf created
      `Prelude.seq` Prelude.rnf authentication
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf notificationDestinations
