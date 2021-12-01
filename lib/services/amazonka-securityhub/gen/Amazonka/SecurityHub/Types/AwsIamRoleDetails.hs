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
-- Module      : Amazonka.SecurityHub.Types.AwsIamRoleDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsIamRoleDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsIamAttachedManagedPolicy
import Amazonka.SecurityHub.Types.AwsIamInstanceProfile
import Amazonka.SecurityHub.Types.AwsIamPermissionsBoundary
import Amazonka.SecurityHub.Types.AwsIamRolePolicy

-- | Contains information about an IAM role, including all of the role\'s
-- policies.
--
-- /See:/ 'newAwsIamRoleDetails' smart constructor.
data AwsIamRoleDetails = AwsIamRoleDetails'
  { -- | The maximum session duration (in seconds) that you want to set for the
    -- specified role.
    maxSessionDuration :: Prelude.Maybe Prelude.Int,
    -- | The trust policy that grants permission to assume the role.
    assumeRolePolicyDocument :: Prelude.Maybe Prelude.Text,
    -- | The path to the role.
    path :: Prelude.Maybe Prelude.Text,
    -- | The list of instance profiles that contain this role.
    instanceProfileList :: Prelude.Maybe [AwsIamInstanceProfile],
    -- | Indicates when the role was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    createDate :: Prelude.Maybe Prelude.Text,
    -- | The friendly name that identifies the role.
    roleName :: Prelude.Maybe Prelude.Text,
    -- | The stable and unique string identifying the role.
    roleId :: Prelude.Maybe Prelude.Text,
    permissionsBoundary :: Prelude.Maybe AwsIamPermissionsBoundary,
    -- | The list of inline policies that are embedded in the role.
    rolePolicyList :: Prelude.Maybe [AwsIamRolePolicy],
    -- | The list of the managed policies that are attached to the role.
    attachedManagedPolicies :: Prelude.Maybe [AwsIamAttachedManagedPolicy]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsIamRoleDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxSessionDuration', 'awsIamRoleDetails_maxSessionDuration' - The maximum session duration (in seconds) that you want to set for the
-- specified role.
--
-- 'assumeRolePolicyDocument', 'awsIamRoleDetails_assumeRolePolicyDocument' - The trust policy that grants permission to assume the role.
--
-- 'path', 'awsIamRoleDetails_path' - The path to the role.
--
-- 'instanceProfileList', 'awsIamRoleDetails_instanceProfileList' - The list of instance profiles that contain this role.
--
-- 'createDate', 'awsIamRoleDetails_createDate' - Indicates when the role was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'roleName', 'awsIamRoleDetails_roleName' - The friendly name that identifies the role.
--
-- 'roleId', 'awsIamRoleDetails_roleId' - The stable and unique string identifying the role.
--
-- 'permissionsBoundary', 'awsIamRoleDetails_permissionsBoundary' - Undocumented member.
--
-- 'rolePolicyList', 'awsIamRoleDetails_rolePolicyList' - The list of inline policies that are embedded in the role.
--
-- 'attachedManagedPolicies', 'awsIamRoleDetails_attachedManagedPolicies' - The list of the managed policies that are attached to the role.
newAwsIamRoleDetails ::
  AwsIamRoleDetails
newAwsIamRoleDetails =
  AwsIamRoleDetails'
    { maxSessionDuration =
        Prelude.Nothing,
      assumeRolePolicyDocument = Prelude.Nothing,
      path = Prelude.Nothing,
      instanceProfileList = Prelude.Nothing,
      createDate = Prelude.Nothing,
      roleName = Prelude.Nothing,
      roleId = Prelude.Nothing,
      permissionsBoundary = Prelude.Nothing,
      rolePolicyList = Prelude.Nothing,
      attachedManagedPolicies = Prelude.Nothing
    }

-- | The maximum session duration (in seconds) that you want to set for the
-- specified role.
awsIamRoleDetails_maxSessionDuration :: Lens.Lens' AwsIamRoleDetails (Prelude.Maybe Prelude.Int)
awsIamRoleDetails_maxSessionDuration = Lens.lens (\AwsIamRoleDetails' {maxSessionDuration} -> maxSessionDuration) (\s@AwsIamRoleDetails' {} a -> s {maxSessionDuration = a} :: AwsIamRoleDetails)

-- | The trust policy that grants permission to assume the role.
awsIamRoleDetails_assumeRolePolicyDocument :: Lens.Lens' AwsIamRoleDetails (Prelude.Maybe Prelude.Text)
awsIamRoleDetails_assumeRolePolicyDocument = Lens.lens (\AwsIamRoleDetails' {assumeRolePolicyDocument} -> assumeRolePolicyDocument) (\s@AwsIamRoleDetails' {} a -> s {assumeRolePolicyDocument = a} :: AwsIamRoleDetails)

-- | The path to the role.
awsIamRoleDetails_path :: Lens.Lens' AwsIamRoleDetails (Prelude.Maybe Prelude.Text)
awsIamRoleDetails_path = Lens.lens (\AwsIamRoleDetails' {path} -> path) (\s@AwsIamRoleDetails' {} a -> s {path = a} :: AwsIamRoleDetails)

-- | The list of instance profiles that contain this role.
awsIamRoleDetails_instanceProfileList :: Lens.Lens' AwsIamRoleDetails (Prelude.Maybe [AwsIamInstanceProfile])
awsIamRoleDetails_instanceProfileList = Lens.lens (\AwsIamRoleDetails' {instanceProfileList} -> instanceProfileList) (\s@AwsIamRoleDetails' {} a -> s {instanceProfileList = a} :: AwsIamRoleDetails) Prelude.. Lens.mapping Lens.coerced

-- | Indicates when the role was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsIamRoleDetails_createDate :: Lens.Lens' AwsIamRoleDetails (Prelude.Maybe Prelude.Text)
awsIamRoleDetails_createDate = Lens.lens (\AwsIamRoleDetails' {createDate} -> createDate) (\s@AwsIamRoleDetails' {} a -> s {createDate = a} :: AwsIamRoleDetails)

-- | The friendly name that identifies the role.
awsIamRoleDetails_roleName :: Lens.Lens' AwsIamRoleDetails (Prelude.Maybe Prelude.Text)
awsIamRoleDetails_roleName = Lens.lens (\AwsIamRoleDetails' {roleName} -> roleName) (\s@AwsIamRoleDetails' {} a -> s {roleName = a} :: AwsIamRoleDetails)

-- | The stable and unique string identifying the role.
awsIamRoleDetails_roleId :: Lens.Lens' AwsIamRoleDetails (Prelude.Maybe Prelude.Text)
awsIamRoleDetails_roleId = Lens.lens (\AwsIamRoleDetails' {roleId} -> roleId) (\s@AwsIamRoleDetails' {} a -> s {roleId = a} :: AwsIamRoleDetails)

-- | Undocumented member.
awsIamRoleDetails_permissionsBoundary :: Lens.Lens' AwsIamRoleDetails (Prelude.Maybe AwsIamPermissionsBoundary)
awsIamRoleDetails_permissionsBoundary = Lens.lens (\AwsIamRoleDetails' {permissionsBoundary} -> permissionsBoundary) (\s@AwsIamRoleDetails' {} a -> s {permissionsBoundary = a} :: AwsIamRoleDetails)

-- | The list of inline policies that are embedded in the role.
awsIamRoleDetails_rolePolicyList :: Lens.Lens' AwsIamRoleDetails (Prelude.Maybe [AwsIamRolePolicy])
awsIamRoleDetails_rolePolicyList = Lens.lens (\AwsIamRoleDetails' {rolePolicyList} -> rolePolicyList) (\s@AwsIamRoleDetails' {} a -> s {rolePolicyList = a} :: AwsIamRoleDetails) Prelude.. Lens.mapping Lens.coerced

-- | The list of the managed policies that are attached to the role.
awsIamRoleDetails_attachedManagedPolicies :: Lens.Lens' AwsIamRoleDetails (Prelude.Maybe [AwsIamAttachedManagedPolicy])
awsIamRoleDetails_attachedManagedPolicies = Lens.lens (\AwsIamRoleDetails' {attachedManagedPolicies} -> attachedManagedPolicies) (\s@AwsIamRoleDetails' {} a -> s {attachedManagedPolicies = a} :: AwsIamRoleDetails) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON AwsIamRoleDetails where
  parseJSON =
    Core.withObject
      "AwsIamRoleDetails"
      ( \x ->
          AwsIamRoleDetails'
            Prelude.<$> (x Core..:? "MaxSessionDuration")
            Prelude.<*> (x Core..:? "AssumeRolePolicyDocument")
            Prelude.<*> (x Core..:? "Path")
            Prelude.<*> ( x Core..:? "InstanceProfileList"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "CreateDate")
            Prelude.<*> (x Core..:? "RoleName")
            Prelude.<*> (x Core..:? "RoleId")
            Prelude.<*> (x Core..:? "PermissionsBoundary")
            Prelude.<*> (x Core..:? "RolePolicyList" Core..!= Prelude.mempty)
            Prelude.<*> ( x Core..:? "AttachedManagedPolicies"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AwsIamRoleDetails where
  hashWithSalt salt' AwsIamRoleDetails' {..} =
    salt'
      `Prelude.hashWithSalt` attachedManagedPolicies
      `Prelude.hashWithSalt` rolePolicyList
      `Prelude.hashWithSalt` permissionsBoundary
      `Prelude.hashWithSalt` roleId
      `Prelude.hashWithSalt` roleName
      `Prelude.hashWithSalt` createDate
      `Prelude.hashWithSalt` instanceProfileList
      `Prelude.hashWithSalt` path
      `Prelude.hashWithSalt` assumeRolePolicyDocument
      `Prelude.hashWithSalt` maxSessionDuration

instance Prelude.NFData AwsIamRoleDetails where
  rnf AwsIamRoleDetails' {..} =
    Prelude.rnf maxSessionDuration
      `Prelude.seq` Prelude.rnf attachedManagedPolicies
      `Prelude.seq` Prelude.rnf rolePolicyList
      `Prelude.seq` Prelude.rnf permissionsBoundary
      `Prelude.seq` Prelude.rnf roleId
      `Prelude.seq` Prelude.rnf roleName
      `Prelude.seq` Prelude.rnf createDate
      `Prelude.seq` Prelude.rnf instanceProfileList
      `Prelude.seq` Prelude.rnf path
      `Prelude.seq` Prelude.rnf assumeRolePolicyDocument

instance Core.ToJSON AwsIamRoleDetails where
  toJSON AwsIamRoleDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MaxSessionDuration" Core..=)
              Prelude.<$> maxSessionDuration,
            ("AssumeRolePolicyDocument" Core..=)
              Prelude.<$> assumeRolePolicyDocument,
            ("Path" Core..=) Prelude.<$> path,
            ("InstanceProfileList" Core..=)
              Prelude.<$> instanceProfileList,
            ("CreateDate" Core..=) Prelude.<$> createDate,
            ("RoleName" Core..=) Prelude.<$> roleName,
            ("RoleId" Core..=) Prelude.<$> roleId,
            ("PermissionsBoundary" Core..=)
              Prelude.<$> permissionsBoundary,
            ("RolePolicyList" Core..=)
              Prelude.<$> rolePolicyList,
            ("AttachedManagedPolicies" Core..=)
              Prelude.<$> attachedManagedPolicies
          ]
      )
