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
-- Module      : Amazonka.MQ.Types.UserPendingChanges
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MQ.Types.UserPendingChanges where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import Amazonka.MQ.Types.ChangeType
import qualified Amazonka.Prelude as Prelude

-- | Returns information about the status of the changes pending for the
-- ActiveMQ user.
--
-- /See:/ 'newUserPendingChanges' smart constructor.
data UserPendingChanges = UserPendingChanges'
  { -- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This
    -- value can contain only alphanumeric characters, dashes, periods,
    -- underscores, and tildes (- . _ ~). This value must be 2-100 characters
    -- long.
    groups :: Prelude.Maybe [Prelude.Text],
    -- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
    consoleAccess :: Prelude.Maybe Prelude.Bool,
    -- | Required. The type of change pending for the ActiveMQ user.
    pendingChange :: ChangeType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserPendingChanges' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groups', 'userPendingChanges_groups' - The list of groups (20 maximum) to which the ActiveMQ user belongs. This
-- value can contain only alphanumeric characters, dashes, periods,
-- underscores, and tildes (- . _ ~). This value must be 2-100 characters
-- long.
--
-- 'consoleAccess', 'userPendingChanges_consoleAccess' - Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
--
-- 'pendingChange', 'userPendingChanges_pendingChange' - Required. The type of change pending for the ActiveMQ user.
newUserPendingChanges ::
  -- | 'pendingChange'
  ChangeType ->
  UserPendingChanges
newUserPendingChanges pPendingChange_ =
  UserPendingChanges'
    { groups = Prelude.Nothing,
      consoleAccess = Prelude.Nothing,
      pendingChange = pPendingChange_
    }

-- | The list of groups (20 maximum) to which the ActiveMQ user belongs. This
-- value can contain only alphanumeric characters, dashes, periods,
-- underscores, and tildes (- . _ ~). This value must be 2-100 characters
-- long.
userPendingChanges_groups :: Lens.Lens' UserPendingChanges (Prelude.Maybe [Prelude.Text])
userPendingChanges_groups = Lens.lens (\UserPendingChanges' {groups} -> groups) (\s@UserPendingChanges' {} a -> s {groups = a} :: UserPendingChanges) Prelude.. Lens.mapping Lens.coerced

-- | Enables access to the the ActiveMQ Web Console for the ActiveMQ user.
userPendingChanges_consoleAccess :: Lens.Lens' UserPendingChanges (Prelude.Maybe Prelude.Bool)
userPendingChanges_consoleAccess = Lens.lens (\UserPendingChanges' {consoleAccess} -> consoleAccess) (\s@UserPendingChanges' {} a -> s {consoleAccess = a} :: UserPendingChanges)

-- | Required. The type of change pending for the ActiveMQ user.
userPendingChanges_pendingChange :: Lens.Lens' UserPendingChanges ChangeType
userPendingChanges_pendingChange = Lens.lens (\UserPendingChanges' {pendingChange} -> pendingChange) (\s@UserPendingChanges' {} a -> s {pendingChange = a} :: UserPendingChanges)

instance Core.FromJSON UserPendingChanges where
  parseJSON =
    Core.withObject
      "UserPendingChanges"
      ( \x ->
          UserPendingChanges'
            Prelude.<$> (x Core..:? "groups" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "consoleAccess")
            Prelude.<*> (x Core..: "pendingChange")
      )

instance Prelude.Hashable UserPendingChanges where
  hashWithSalt salt' UserPendingChanges' {..} =
    salt' `Prelude.hashWithSalt` pendingChange
      `Prelude.hashWithSalt` consoleAccess
      `Prelude.hashWithSalt` groups

instance Prelude.NFData UserPendingChanges where
  rnf UserPendingChanges' {..} =
    Prelude.rnf groups
      `Prelude.seq` Prelude.rnf pendingChange
      `Prelude.seq` Prelude.rnf consoleAccess
