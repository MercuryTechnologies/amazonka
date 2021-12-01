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
-- Module      : Amazonka.SecurityHub.Types.AwsIamGroupDetails
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsIamGroupDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsIamAttachedManagedPolicy
import Amazonka.SecurityHub.Types.AwsIamGroupPolicy

-- | Contains details about an IAM group.
--
-- /See:/ 'newAwsIamGroupDetails' smart constructor.
data AwsIamGroupDetails = AwsIamGroupDetails'
  { -- | The path to the group.
    path :: Prelude.Maybe Prelude.Text,
    -- | Indicates when the IAM group was created.
    --
    -- Uses the @date-time@ format specified in
    -- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
    -- The value cannot contain spaces. For example,
    -- @2020-03-22T13:22:13.933Z@.
    createDate :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the IAM group.
    groupId :: Prelude.Maybe Prelude.Text,
    -- | The list of inline policies that are embedded in the group.
    groupPolicyList :: Prelude.Maybe [AwsIamGroupPolicy],
    -- | The name of the IAM group.
    groupName :: Prelude.Maybe Prelude.Text,
    -- | A list of the managed policies that are attached to the IAM group.
    attachedManagedPolicies :: Prelude.Maybe [AwsIamAttachedManagedPolicy]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsIamGroupDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'path', 'awsIamGroupDetails_path' - The path to the group.
--
-- 'createDate', 'awsIamGroupDetails_createDate' - Indicates when the IAM group was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
--
-- 'groupId', 'awsIamGroupDetails_groupId' - The identifier of the IAM group.
--
-- 'groupPolicyList', 'awsIamGroupDetails_groupPolicyList' - The list of inline policies that are embedded in the group.
--
-- 'groupName', 'awsIamGroupDetails_groupName' - The name of the IAM group.
--
-- 'attachedManagedPolicies', 'awsIamGroupDetails_attachedManagedPolicies' - A list of the managed policies that are attached to the IAM group.
newAwsIamGroupDetails ::
  AwsIamGroupDetails
newAwsIamGroupDetails =
  AwsIamGroupDetails'
    { path = Prelude.Nothing,
      createDate = Prelude.Nothing,
      groupId = Prelude.Nothing,
      groupPolicyList = Prelude.Nothing,
      groupName = Prelude.Nothing,
      attachedManagedPolicies = Prelude.Nothing
    }

-- | The path to the group.
awsIamGroupDetails_path :: Lens.Lens' AwsIamGroupDetails (Prelude.Maybe Prelude.Text)
awsIamGroupDetails_path = Lens.lens (\AwsIamGroupDetails' {path} -> path) (\s@AwsIamGroupDetails' {} a -> s {path = a} :: AwsIamGroupDetails)

-- | Indicates when the IAM group was created.
--
-- Uses the @date-time@ format specified in
-- <https://tools.ietf.org/html/rfc3339#section-5.6 RFC 3339 section 5.6, Internet Date\/Time Format>.
-- The value cannot contain spaces. For example,
-- @2020-03-22T13:22:13.933Z@.
awsIamGroupDetails_createDate :: Lens.Lens' AwsIamGroupDetails (Prelude.Maybe Prelude.Text)
awsIamGroupDetails_createDate = Lens.lens (\AwsIamGroupDetails' {createDate} -> createDate) (\s@AwsIamGroupDetails' {} a -> s {createDate = a} :: AwsIamGroupDetails)

-- | The identifier of the IAM group.
awsIamGroupDetails_groupId :: Lens.Lens' AwsIamGroupDetails (Prelude.Maybe Prelude.Text)
awsIamGroupDetails_groupId = Lens.lens (\AwsIamGroupDetails' {groupId} -> groupId) (\s@AwsIamGroupDetails' {} a -> s {groupId = a} :: AwsIamGroupDetails)

-- | The list of inline policies that are embedded in the group.
awsIamGroupDetails_groupPolicyList :: Lens.Lens' AwsIamGroupDetails (Prelude.Maybe [AwsIamGroupPolicy])
awsIamGroupDetails_groupPolicyList = Lens.lens (\AwsIamGroupDetails' {groupPolicyList} -> groupPolicyList) (\s@AwsIamGroupDetails' {} a -> s {groupPolicyList = a} :: AwsIamGroupDetails) Prelude.. Lens.mapping Lens.coerced

-- | The name of the IAM group.
awsIamGroupDetails_groupName :: Lens.Lens' AwsIamGroupDetails (Prelude.Maybe Prelude.Text)
awsIamGroupDetails_groupName = Lens.lens (\AwsIamGroupDetails' {groupName} -> groupName) (\s@AwsIamGroupDetails' {} a -> s {groupName = a} :: AwsIamGroupDetails)

-- | A list of the managed policies that are attached to the IAM group.
awsIamGroupDetails_attachedManagedPolicies :: Lens.Lens' AwsIamGroupDetails (Prelude.Maybe [AwsIamAttachedManagedPolicy])
awsIamGroupDetails_attachedManagedPolicies = Lens.lens (\AwsIamGroupDetails' {attachedManagedPolicies} -> attachedManagedPolicies) (\s@AwsIamGroupDetails' {} a -> s {attachedManagedPolicies = a} :: AwsIamGroupDetails) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON AwsIamGroupDetails where
  parseJSON =
    Core.withObject
      "AwsIamGroupDetails"
      ( \x ->
          AwsIamGroupDetails'
            Prelude.<$> (x Core..:? "Path")
            Prelude.<*> (x Core..:? "CreateDate")
            Prelude.<*> (x Core..:? "GroupId")
            Prelude.<*> ( x Core..:? "GroupPolicyList"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "GroupName")
            Prelude.<*> ( x Core..:? "AttachedManagedPolicies"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable AwsIamGroupDetails where
  hashWithSalt salt' AwsIamGroupDetails' {..} =
    salt'
      `Prelude.hashWithSalt` attachedManagedPolicies
      `Prelude.hashWithSalt` groupName
      `Prelude.hashWithSalt` groupPolicyList
      `Prelude.hashWithSalt` groupId
      `Prelude.hashWithSalt` createDate
      `Prelude.hashWithSalt` path

instance Prelude.NFData AwsIamGroupDetails where
  rnf AwsIamGroupDetails' {..} =
    Prelude.rnf path
      `Prelude.seq` Prelude.rnf attachedManagedPolicies
      `Prelude.seq` Prelude.rnf groupName
      `Prelude.seq` Prelude.rnf groupPolicyList
      `Prelude.seq` Prelude.rnf groupId
      `Prelude.seq` Prelude.rnf createDate

instance Core.ToJSON AwsIamGroupDetails where
  toJSON AwsIamGroupDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Path" Core..=) Prelude.<$> path,
            ("CreateDate" Core..=) Prelude.<$> createDate,
            ("GroupId" Core..=) Prelude.<$> groupId,
            ("GroupPolicyList" Core..=)
              Prelude.<$> groupPolicyList,
            ("GroupName" Core..=) Prelude.<$> groupName,
            ("AttachedManagedPolicies" Core..=)
              Prelude.<$> attachedManagedPolicies
          ]
      )
