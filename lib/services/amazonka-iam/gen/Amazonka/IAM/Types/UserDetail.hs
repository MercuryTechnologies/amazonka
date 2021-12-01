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
-- Module      : Amazonka.IAM.Types.UserDetail
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.UserDetail where

import qualified Amazonka.Core as Core
import Amazonka.IAM.Types.AttachedPermissionsBoundary
import Amazonka.IAM.Types.AttachedPolicy
import Amazonka.IAM.Types.PolicyDetail
import Amazonka.IAM.Types.Tag
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude

-- | Contains information about an IAM user, including all the user\'s
-- policies and all the IAM groups the user is in.
--
-- This data type is used as a response element in the
-- GetAccountAuthorizationDetails operation.
--
-- /See:/ 'newUserDetail' smart constructor.
data UserDetail = UserDetail'
  { -- | A list of IAM groups that the user is in.
    groupList :: Prelude.Maybe [Prelude.Text],
    arn :: Prelude.Maybe Prelude.Text,
    -- | The path to the user. For more information about paths, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    path :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in
    -- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
    -- user was created.
    createDate :: Prelude.Maybe Core.ISO8601,
    -- | The friendly name identifying the user.
    userName :: Prelude.Maybe Prelude.Text,
    -- | The stable and unique string identifying the user. For more information
    -- about IDs, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
    -- in the /IAM User Guide/.
    userId :: Prelude.Maybe Prelude.Text,
    -- | The ARN of the policy used to set the permissions boundary for the user.
    --
    -- For more information about permissions boundaries, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
    -- in the /IAM User Guide/.
    permissionsBoundary :: Prelude.Maybe AttachedPermissionsBoundary,
    -- | A list of the inline policies embedded in the user.
    userPolicyList :: Prelude.Maybe [PolicyDetail],
    -- | A list of tags that are associated with the user. For more information
    -- about tagging, see
    -- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
    -- in the /IAM User Guide/.
    tags :: Prelude.Maybe [Tag],
    -- | A list of the managed policies attached to the user.
    attachedManagedPolicies :: Prelude.Maybe [AttachedPolicy]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserDetail' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groupList', 'userDetail_groupList' - A list of IAM groups that the user is in.
--
-- 'arn', 'userDetail_arn' - Undocumented member.
--
-- 'path', 'userDetail_path' - The path to the user. For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- 'createDate', 'userDetail_createDate' - The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- user was created.
--
-- 'userName', 'userDetail_userName' - The friendly name identifying the user.
--
-- 'userId', 'userDetail_userId' - The stable and unique string identifying the user. For more information
-- about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
--
-- 'permissionsBoundary', 'userDetail_permissionsBoundary' - The ARN of the policy used to set the permissions boundary for the user.
--
-- For more information about permissions boundaries, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
-- in the /IAM User Guide/.
--
-- 'userPolicyList', 'userDetail_userPolicyList' - A list of the inline policies embedded in the user.
--
-- 'tags', 'userDetail_tags' - A list of tags that are associated with the user. For more information
-- about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
--
-- 'attachedManagedPolicies', 'userDetail_attachedManagedPolicies' - A list of the managed policies attached to the user.
newUserDetail ::
  UserDetail
newUserDetail =
  UserDetail'
    { groupList = Prelude.Nothing,
      arn = Prelude.Nothing,
      path = Prelude.Nothing,
      createDate = Prelude.Nothing,
      userName = Prelude.Nothing,
      userId = Prelude.Nothing,
      permissionsBoundary = Prelude.Nothing,
      userPolicyList = Prelude.Nothing,
      tags = Prelude.Nothing,
      attachedManagedPolicies = Prelude.Nothing
    }

-- | A list of IAM groups that the user is in.
userDetail_groupList :: Lens.Lens' UserDetail (Prelude.Maybe [Prelude.Text])
userDetail_groupList = Lens.lens (\UserDetail' {groupList} -> groupList) (\s@UserDetail' {} a -> s {groupList = a} :: UserDetail) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
userDetail_arn :: Lens.Lens' UserDetail (Prelude.Maybe Prelude.Text)
userDetail_arn = Lens.lens (\UserDetail' {arn} -> arn) (\s@UserDetail' {} a -> s {arn = a} :: UserDetail)

-- | The path to the user. For more information about paths, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
userDetail_path :: Lens.Lens' UserDetail (Prelude.Maybe Prelude.Text)
userDetail_path = Lens.lens (\UserDetail' {path} -> path) (\s@UserDetail' {} a -> s {path = a} :: UserDetail)

-- | The date and time, in
-- <http://www.iso.org/iso/iso8601 ISO 8601 date-time format>, when the
-- user was created.
userDetail_createDate :: Lens.Lens' UserDetail (Prelude.Maybe Prelude.UTCTime)
userDetail_createDate = Lens.lens (\UserDetail' {createDate} -> createDate) (\s@UserDetail' {} a -> s {createDate = a} :: UserDetail) Prelude.. Lens.mapping Core._Time

-- | The friendly name identifying the user.
userDetail_userName :: Lens.Lens' UserDetail (Prelude.Maybe Prelude.Text)
userDetail_userName = Lens.lens (\UserDetail' {userName} -> userName) (\s@UserDetail' {} a -> s {userName = a} :: UserDetail)

-- | The stable and unique string identifying the user. For more information
-- about IDs, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_Identifiers.html IAM identifiers>
-- in the /IAM User Guide/.
userDetail_userId :: Lens.Lens' UserDetail (Prelude.Maybe Prelude.Text)
userDetail_userId = Lens.lens (\UserDetail' {userId} -> userId) (\s@UserDetail' {} a -> s {userId = a} :: UserDetail)

-- | The ARN of the policy used to set the permissions boundary for the user.
--
-- For more information about permissions boundaries, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access_policies_boundaries.html Permissions boundaries for IAM identities>
-- in the /IAM User Guide/.
userDetail_permissionsBoundary :: Lens.Lens' UserDetail (Prelude.Maybe AttachedPermissionsBoundary)
userDetail_permissionsBoundary = Lens.lens (\UserDetail' {permissionsBoundary} -> permissionsBoundary) (\s@UserDetail' {} a -> s {permissionsBoundary = a} :: UserDetail)

-- | A list of the inline policies embedded in the user.
userDetail_userPolicyList :: Lens.Lens' UserDetail (Prelude.Maybe [PolicyDetail])
userDetail_userPolicyList = Lens.lens (\UserDetail' {userPolicyList} -> userPolicyList) (\s@UserDetail' {} a -> s {userPolicyList = a} :: UserDetail) Prelude.. Lens.mapping Lens.coerced

-- | A list of tags that are associated with the user. For more information
-- about tagging, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_tags.html Tagging IAM resources>
-- in the /IAM User Guide/.
userDetail_tags :: Lens.Lens' UserDetail (Prelude.Maybe [Tag])
userDetail_tags = Lens.lens (\UserDetail' {tags} -> tags) (\s@UserDetail' {} a -> s {tags = a} :: UserDetail) Prelude.. Lens.mapping Lens.coerced

-- | A list of the managed policies attached to the user.
userDetail_attachedManagedPolicies :: Lens.Lens' UserDetail (Prelude.Maybe [AttachedPolicy])
userDetail_attachedManagedPolicies = Lens.lens (\UserDetail' {attachedManagedPolicies} -> attachedManagedPolicies) (\s@UserDetail' {} a -> s {attachedManagedPolicies = a} :: UserDetail) Prelude.. Lens.mapping Lens.coerced

instance Core.FromXML UserDetail where
  parseXML x =
    UserDetail'
      Prelude.<$> ( x Core..@? "GroupList" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> (x Core..@? "Arn")
      Prelude.<*> (x Core..@? "Path")
      Prelude.<*> (x Core..@? "CreateDate")
      Prelude.<*> (x Core..@? "UserName")
      Prelude.<*> (x Core..@? "UserId")
      Prelude.<*> (x Core..@? "PermissionsBoundary")
      Prelude.<*> ( x Core..@? "UserPolicyList" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> ( x Core..@? "Tags" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )
      Prelude.<*> ( x Core..@? "AttachedManagedPolicies"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "member")
                  )

instance Prelude.Hashable UserDetail where
  hashWithSalt salt' UserDetail' {..} =
    salt'
      `Prelude.hashWithSalt` attachedManagedPolicies
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` userPolicyList
      `Prelude.hashWithSalt` permissionsBoundary
      `Prelude.hashWithSalt` userId
      `Prelude.hashWithSalt` userName
      `Prelude.hashWithSalt` createDate
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` groupList

instance Prelude.NFData UserDetail where
  rnf UserDetail' {..} =
    Prelude.rnf groupList
      `Prelude.seq` Prelude.rnf attachedManagedPolicies
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf userPolicyList
      `Prelude.seq` Prelude.rnf permissionsBoundary
      `Prelude.seq` Prelude.rnf userId
      `Prelude.seq` Prelude.rnf userName
      `Prelude.seq` Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf arn
