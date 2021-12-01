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
-- Module      : Amazonka.ElastiCache.Types.UserGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.ElastiCache.Types.UserGroup where

import qualified Amazonka.Core as Core
import Amazonka.ElastiCache.Types.UserGroupPendingChanges
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | /See:/ 'newUserGroup' smart constructor.
data UserGroup = UserGroup'
  { -- | Indicates user group status. Can be \"creating\", \"active\",
    -- \"modifying\", \"deleting\".
    status :: Prelude.Maybe Prelude.Text,
    -- | The list of user IDs that belong to the user group.
    userIds :: Prelude.Maybe [Prelude.Text],
    -- | The Amazon Resource Name (ARN) of the user group.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the user group.
    userGroupId :: Prelude.Maybe Prelude.Text,
    -- | The current supported value is Redis.
    engine :: Prelude.Maybe Prelude.Text,
    -- | A list of updates being applied to the user group.
    pendingChanges :: Prelude.Maybe UserGroupPendingChanges,
    -- | A list of replication groups that the user group can access.
    replicationGroups :: Prelude.Maybe [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'userGroup_status' - Indicates user group status. Can be \"creating\", \"active\",
-- \"modifying\", \"deleting\".
--
-- 'userIds', 'userGroup_userIds' - The list of user IDs that belong to the user group.
--
-- 'arn', 'userGroup_arn' - The Amazon Resource Name (ARN) of the user group.
--
-- 'userGroupId', 'userGroup_userGroupId' - The ID of the user group.
--
-- 'engine', 'userGroup_engine' - The current supported value is Redis.
--
-- 'pendingChanges', 'userGroup_pendingChanges' - A list of updates being applied to the user group.
--
-- 'replicationGroups', 'userGroup_replicationGroups' - A list of replication groups that the user group can access.
newUserGroup ::
  UserGroup
newUserGroup =
  UserGroup'
    { status = Prelude.Nothing,
      userIds = Prelude.Nothing,
      arn = Prelude.Nothing,
      userGroupId = Prelude.Nothing,
      engine = Prelude.Nothing,
      pendingChanges = Prelude.Nothing,
      replicationGroups = Prelude.Nothing
    }

-- | Indicates user group status. Can be \"creating\", \"active\",
-- \"modifying\", \"deleting\".
userGroup_status :: Lens.Lens' UserGroup (Prelude.Maybe Prelude.Text)
userGroup_status = Lens.lens (\UserGroup' {status} -> status) (\s@UserGroup' {} a -> s {status = a} :: UserGroup)

-- | The list of user IDs that belong to the user group.
userGroup_userIds :: Lens.Lens' UserGroup (Prelude.Maybe [Prelude.Text])
userGroup_userIds = Lens.lens (\UserGroup' {userIds} -> userIds) (\s@UserGroup' {} a -> s {userIds = a} :: UserGroup) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the user group.
userGroup_arn :: Lens.Lens' UserGroup (Prelude.Maybe Prelude.Text)
userGroup_arn = Lens.lens (\UserGroup' {arn} -> arn) (\s@UserGroup' {} a -> s {arn = a} :: UserGroup)

-- | The ID of the user group.
userGroup_userGroupId :: Lens.Lens' UserGroup (Prelude.Maybe Prelude.Text)
userGroup_userGroupId = Lens.lens (\UserGroup' {userGroupId} -> userGroupId) (\s@UserGroup' {} a -> s {userGroupId = a} :: UserGroup)

-- | The current supported value is Redis.
userGroup_engine :: Lens.Lens' UserGroup (Prelude.Maybe Prelude.Text)
userGroup_engine = Lens.lens (\UserGroup' {engine} -> engine) (\s@UserGroup' {} a -> s {engine = a} :: UserGroup)

-- | A list of updates being applied to the user group.
userGroup_pendingChanges :: Lens.Lens' UserGroup (Prelude.Maybe UserGroupPendingChanges)
userGroup_pendingChanges = Lens.lens (\UserGroup' {pendingChanges} -> pendingChanges) (\s@UserGroup' {} a -> s {pendingChanges = a} :: UserGroup)

-- | A list of replication groups that the user group can access.
userGroup_replicationGroups :: Lens.Lens' UserGroup (Prelude.Maybe [Prelude.Text])
userGroup_replicationGroups = Lens.lens (\UserGroup' {replicationGroups} -> replicationGroups) (\s@UserGroup' {} a -> s {replicationGroups = a} :: UserGroup) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML UserGroup where
  parseXML x =
    UserGroup'
      Prelude.<$> (x Core..@? "Status")
      Prelude.<*> ( x Core..@? "UserIds" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "ARN")
      Prelude.<*> (x Core..@? "UserGroupId")
      Prelude.<*> (x Core..@? "Engine")
      Prelude.<*> (x Core..@? "PendingChanges")
      Prelude.<*> ( x Core..@? "ReplicationGroups"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )

instance Prelude.Hashable UserGroup where
  hashWithSalt salt' UserGroup' {..} =
    salt' `Prelude.hashWithSalt` replicationGroups
      `Prelude.hashWithSalt` pendingChanges
      `Prelude.hashWithSalt` engine
      `Prelude.hashWithSalt` userGroupId
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` userIds
      `Prelude.hashWithSalt` status

instance Prelude.NFData UserGroup where
  rnf UserGroup' {..} =
    Prelude.rnf status
      `Prelude.seq` Prelude.rnf replicationGroups
      `Prelude.seq` Prelude.rnf pendingChanges
      `Prelude.seq` Prelude.rnf engine
      `Prelude.seq` Prelude.rnf userGroupId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf userIds
