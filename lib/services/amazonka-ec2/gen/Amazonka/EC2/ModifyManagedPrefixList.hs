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
-- Module      : Amazonka.EC2.ModifyManagedPrefixList
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified managed prefix list.
--
-- Adding or removing entries in a prefix list creates a new version of the
-- prefix list. Changing the name of the prefix list does not affect the
-- version.
--
-- If you specify a current version number that does not match the true
-- current version number, the request fails.
module Amazonka.EC2.ModifyManagedPrefixList
  ( -- * Creating a Request
    ModifyManagedPrefixList (..),
    newModifyManagedPrefixList,

    -- * Request Lenses
    modifyManagedPrefixList_currentVersion,
    modifyManagedPrefixList_removeEntries,
    modifyManagedPrefixList_prefixListName,
    modifyManagedPrefixList_addEntries,
    modifyManagedPrefixList_maxEntries,
    modifyManagedPrefixList_dryRun,
    modifyManagedPrefixList_prefixListId,

    -- * Destructuring the Response
    ModifyManagedPrefixListResponse (..),
    newModifyManagedPrefixListResponse,

    -- * Response Lenses
    modifyManagedPrefixListResponse_prefixList,
    modifyManagedPrefixListResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.EC2.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newModifyManagedPrefixList' smart constructor.
data ModifyManagedPrefixList = ModifyManagedPrefixList'
  { -- | The current version of the prefix list.
    currentVersion :: Prelude.Maybe Prelude.Integer,
    -- | One or more entries to remove from the prefix list.
    removeEntries :: Prelude.Maybe [RemovePrefixListEntry],
    -- | A name for the prefix list.
    prefixListName :: Prelude.Maybe Prelude.Text,
    -- | One or more entries to add to the prefix list.
    addEntries :: Prelude.Maybe [AddPrefixListEntry],
    -- | The maximum number of entries for the prefix list. You cannot modify the
    -- entries of a prefix list and modify the size of a prefix list at the
    -- same time.
    --
    -- If any of the resources that reference the prefix list cannot support
    -- the new maximum size, the modify operation fails. Check the state
    -- message for the IDs of the first ten resources that do not support the
    -- new maximum size.
    maxEntries :: Prelude.Maybe Prelude.Int,
    -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the prefix list.
    prefixListId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyManagedPrefixList' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'currentVersion', 'modifyManagedPrefixList_currentVersion' - The current version of the prefix list.
--
-- 'removeEntries', 'modifyManagedPrefixList_removeEntries' - One or more entries to remove from the prefix list.
--
-- 'prefixListName', 'modifyManagedPrefixList_prefixListName' - A name for the prefix list.
--
-- 'addEntries', 'modifyManagedPrefixList_addEntries' - One or more entries to add to the prefix list.
--
-- 'maxEntries', 'modifyManagedPrefixList_maxEntries' - The maximum number of entries for the prefix list. You cannot modify the
-- entries of a prefix list and modify the size of a prefix list at the
-- same time.
--
-- If any of the resources that reference the prefix list cannot support
-- the new maximum size, the modify operation fails. Check the state
-- message for the IDs of the first ten resources that do not support the
-- new maximum size.
--
-- 'dryRun', 'modifyManagedPrefixList_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'prefixListId', 'modifyManagedPrefixList_prefixListId' - The ID of the prefix list.
newModifyManagedPrefixList ::
  -- | 'prefixListId'
  Prelude.Text ->
  ModifyManagedPrefixList
newModifyManagedPrefixList pPrefixListId_ =
  ModifyManagedPrefixList'
    { currentVersion =
        Prelude.Nothing,
      removeEntries = Prelude.Nothing,
      prefixListName = Prelude.Nothing,
      addEntries = Prelude.Nothing,
      maxEntries = Prelude.Nothing,
      dryRun = Prelude.Nothing,
      prefixListId = pPrefixListId_
    }

-- | The current version of the prefix list.
modifyManagedPrefixList_currentVersion :: Lens.Lens' ModifyManagedPrefixList (Prelude.Maybe Prelude.Integer)
modifyManagedPrefixList_currentVersion = Lens.lens (\ModifyManagedPrefixList' {currentVersion} -> currentVersion) (\s@ModifyManagedPrefixList' {} a -> s {currentVersion = a} :: ModifyManagedPrefixList)

-- | One or more entries to remove from the prefix list.
modifyManagedPrefixList_removeEntries :: Lens.Lens' ModifyManagedPrefixList (Prelude.Maybe [RemovePrefixListEntry])
modifyManagedPrefixList_removeEntries = Lens.lens (\ModifyManagedPrefixList' {removeEntries} -> removeEntries) (\s@ModifyManagedPrefixList' {} a -> s {removeEntries = a} :: ModifyManagedPrefixList) Prelude.. Lens.mapping Lens.coerced

-- | A name for the prefix list.
modifyManagedPrefixList_prefixListName :: Lens.Lens' ModifyManagedPrefixList (Prelude.Maybe Prelude.Text)
modifyManagedPrefixList_prefixListName = Lens.lens (\ModifyManagedPrefixList' {prefixListName} -> prefixListName) (\s@ModifyManagedPrefixList' {} a -> s {prefixListName = a} :: ModifyManagedPrefixList)

-- | One or more entries to add to the prefix list.
modifyManagedPrefixList_addEntries :: Lens.Lens' ModifyManagedPrefixList (Prelude.Maybe [AddPrefixListEntry])
modifyManagedPrefixList_addEntries = Lens.lens (\ModifyManagedPrefixList' {addEntries} -> addEntries) (\s@ModifyManagedPrefixList' {} a -> s {addEntries = a} :: ModifyManagedPrefixList) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of entries for the prefix list. You cannot modify the
-- entries of a prefix list and modify the size of a prefix list at the
-- same time.
--
-- If any of the resources that reference the prefix list cannot support
-- the new maximum size, the modify operation fails. Check the state
-- message for the IDs of the first ten resources that do not support the
-- new maximum size.
modifyManagedPrefixList_maxEntries :: Lens.Lens' ModifyManagedPrefixList (Prelude.Maybe Prelude.Int)
modifyManagedPrefixList_maxEntries = Lens.lens (\ModifyManagedPrefixList' {maxEntries} -> maxEntries) (\s@ModifyManagedPrefixList' {} a -> s {maxEntries = a} :: ModifyManagedPrefixList)

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
modifyManagedPrefixList_dryRun :: Lens.Lens' ModifyManagedPrefixList (Prelude.Maybe Prelude.Bool)
modifyManagedPrefixList_dryRun = Lens.lens (\ModifyManagedPrefixList' {dryRun} -> dryRun) (\s@ModifyManagedPrefixList' {} a -> s {dryRun = a} :: ModifyManagedPrefixList)

-- | The ID of the prefix list.
modifyManagedPrefixList_prefixListId :: Lens.Lens' ModifyManagedPrefixList Prelude.Text
modifyManagedPrefixList_prefixListId = Lens.lens (\ModifyManagedPrefixList' {prefixListId} -> prefixListId) (\s@ModifyManagedPrefixList' {} a -> s {prefixListId = a} :: ModifyManagedPrefixList)

instance Core.AWSRequest ModifyManagedPrefixList where
  type
    AWSResponse ModifyManagedPrefixList =
      ModifyManagedPrefixListResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          ModifyManagedPrefixListResponse'
            Prelude.<$> (x Core..@? "prefixList")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyManagedPrefixList where
  hashWithSalt salt' ModifyManagedPrefixList' {..} =
    salt' `Prelude.hashWithSalt` prefixListId
      `Prelude.hashWithSalt` dryRun
      `Prelude.hashWithSalt` maxEntries
      `Prelude.hashWithSalt` addEntries
      `Prelude.hashWithSalt` prefixListName
      `Prelude.hashWithSalt` removeEntries
      `Prelude.hashWithSalt` currentVersion

instance Prelude.NFData ModifyManagedPrefixList where
  rnf ModifyManagedPrefixList' {..} =
    Prelude.rnf currentVersion
      `Prelude.seq` Prelude.rnf prefixListId
      `Prelude.seq` Prelude.rnf dryRun
      `Prelude.seq` Prelude.rnf maxEntries
      `Prelude.seq` Prelude.rnf addEntries
      `Prelude.seq` Prelude.rnf prefixListName
      `Prelude.seq` Prelude.rnf removeEntries

instance Core.ToHeaders ModifyManagedPrefixList where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ModifyManagedPrefixList where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifyManagedPrefixList where
  toQuery ModifyManagedPrefixList' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ("ModifyManagedPrefixList" :: Prelude.ByteString),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "CurrentVersion" Core.=: currentVersion,
        Core.toQuery
          ( Core.toQueryList "RemoveEntry"
              Prelude.<$> removeEntries
          ),
        "PrefixListName" Core.=: prefixListName,
        Core.toQuery
          (Core.toQueryList "AddEntry" Prelude.<$> addEntries),
        "MaxEntries" Core.=: maxEntries,
        "DryRun" Core.=: dryRun,
        "PrefixListId" Core.=: prefixListId
      ]

-- | /See:/ 'newModifyManagedPrefixListResponse' smart constructor.
data ModifyManagedPrefixListResponse = ModifyManagedPrefixListResponse'
  { -- | Information about the prefix list.
    prefixList :: Prelude.Maybe ManagedPrefixList,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyManagedPrefixListResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'prefixList', 'modifyManagedPrefixListResponse_prefixList' - Information about the prefix list.
--
-- 'httpStatus', 'modifyManagedPrefixListResponse_httpStatus' - The response's http status code.
newModifyManagedPrefixListResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyManagedPrefixListResponse
newModifyManagedPrefixListResponse pHttpStatus_ =
  ModifyManagedPrefixListResponse'
    { prefixList =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the prefix list.
modifyManagedPrefixListResponse_prefixList :: Lens.Lens' ModifyManagedPrefixListResponse (Prelude.Maybe ManagedPrefixList)
modifyManagedPrefixListResponse_prefixList = Lens.lens (\ModifyManagedPrefixListResponse' {prefixList} -> prefixList) (\s@ModifyManagedPrefixListResponse' {} a -> s {prefixList = a} :: ModifyManagedPrefixListResponse)

-- | The response's http status code.
modifyManagedPrefixListResponse_httpStatus :: Lens.Lens' ModifyManagedPrefixListResponse Prelude.Int
modifyManagedPrefixListResponse_httpStatus = Lens.lens (\ModifyManagedPrefixListResponse' {httpStatus} -> httpStatus) (\s@ModifyManagedPrefixListResponse' {} a -> s {httpStatus = a} :: ModifyManagedPrefixListResponse)

instance
  Prelude.NFData
    ModifyManagedPrefixListResponse
  where
  rnf ModifyManagedPrefixListResponse' {..} =
    Prelude.rnf prefixList
      `Prelude.seq` Prelude.rnf httpStatus
