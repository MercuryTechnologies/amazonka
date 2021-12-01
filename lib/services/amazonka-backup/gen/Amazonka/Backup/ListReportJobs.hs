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
-- Module      : Amazonka.Backup.ListReportJobs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns details about your report jobs.
module Amazonka.Backup.ListReportJobs
  ( -- * Creating a Request
    ListReportJobs (..),
    newListReportJobs,

    -- * Request Lenses
    listReportJobs_byStatus,
    listReportJobs_byReportPlanName,
    listReportJobs_nextToken,
    listReportJobs_byCreationBefore,
    listReportJobs_byCreationAfter,
    listReportJobs_maxResults,

    -- * Destructuring the Response
    ListReportJobsResponse (..),
    newListReportJobsResponse,

    -- * Response Lenses
    listReportJobsResponse_reportJobs,
    listReportJobsResponse_nextToken,
    listReportJobsResponse_httpStatus,
  )
where

import Amazonka.Backup.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListReportJobs' smart constructor.
data ListReportJobs = ListReportJobs'
  { -- | Returns only report jobs that are in the specified status. The statuses
    -- are:
    --
    -- @CREATED | RUNNING | COMPLETED | FAILED@
    byStatus :: Prelude.Maybe Prelude.Text,
    -- | Returns only report jobs with the specified report plan name.
    byReportPlanName :: Prelude.Maybe Prelude.Text,
    -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Returns only report jobs that were created before the date and time
    -- specified in Unix format and Coordinated Universal Time (UTC). For
    -- example, the value 1516925490 represents Friday, January 26, 2018
    -- 12:11:30 AM.
    byCreationBefore :: Prelude.Maybe Core.POSIX,
    -- | Returns only report jobs that were created after the date and time
    -- specified in Unix format and Coordinated Universal Time (UTC). For
    -- example, the value 1516925490 represents Friday, January 26, 2018
    -- 12:11:30 AM.
    byCreationAfter :: Prelude.Maybe Core.POSIX,
    -- | The number of desired results from 1 to 1000. Optional. If unspecified,
    -- the query will return 1 MB of data.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReportJobs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'byStatus', 'listReportJobs_byStatus' - Returns only report jobs that are in the specified status. The statuses
-- are:
--
-- @CREATED | RUNNING | COMPLETED | FAILED@
--
-- 'byReportPlanName', 'listReportJobs_byReportPlanName' - Returns only report jobs with the specified report plan name.
--
-- 'nextToken', 'listReportJobs_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'byCreationBefore', 'listReportJobs_byCreationBefore' - Returns only report jobs that were created before the date and time
-- specified in Unix format and Coordinated Universal Time (UTC). For
-- example, the value 1516925490 represents Friday, January 26, 2018
-- 12:11:30 AM.
--
-- 'byCreationAfter', 'listReportJobs_byCreationAfter' - Returns only report jobs that were created after the date and time
-- specified in Unix format and Coordinated Universal Time (UTC). For
-- example, the value 1516925490 represents Friday, January 26, 2018
-- 12:11:30 AM.
--
-- 'maxResults', 'listReportJobs_maxResults' - The number of desired results from 1 to 1000. Optional. If unspecified,
-- the query will return 1 MB of data.
newListReportJobs ::
  ListReportJobs
newListReportJobs =
  ListReportJobs'
    { byStatus = Prelude.Nothing,
      byReportPlanName = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      byCreationBefore = Prelude.Nothing,
      byCreationAfter = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | Returns only report jobs that are in the specified status. The statuses
-- are:
--
-- @CREATED | RUNNING | COMPLETED | FAILED@
listReportJobs_byStatus :: Lens.Lens' ListReportJobs (Prelude.Maybe Prelude.Text)
listReportJobs_byStatus = Lens.lens (\ListReportJobs' {byStatus} -> byStatus) (\s@ListReportJobs' {} a -> s {byStatus = a} :: ListReportJobs)

-- | Returns only report jobs with the specified report plan name.
listReportJobs_byReportPlanName :: Lens.Lens' ListReportJobs (Prelude.Maybe Prelude.Text)
listReportJobs_byReportPlanName = Lens.lens (\ListReportJobs' {byReportPlanName} -> byReportPlanName) (\s@ListReportJobs' {} a -> s {byReportPlanName = a} :: ListReportJobs)

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listReportJobs_nextToken :: Lens.Lens' ListReportJobs (Prelude.Maybe Prelude.Text)
listReportJobs_nextToken = Lens.lens (\ListReportJobs' {nextToken} -> nextToken) (\s@ListReportJobs' {} a -> s {nextToken = a} :: ListReportJobs)

-- | Returns only report jobs that were created before the date and time
-- specified in Unix format and Coordinated Universal Time (UTC). For
-- example, the value 1516925490 represents Friday, January 26, 2018
-- 12:11:30 AM.
listReportJobs_byCreationBefore :: Lens.Lens' ListReportJobs (Prelude.Maybe Prelude.UTCTime)
listReportJobs_byCreationBefore = Lens.lens (\ListReportJobs' {byCreationBefore} -> byCreationBefore) (\s@ListReportJobs' {} a -> s {byCreationBefore = a} :: ListReportJobs) Prelude.. Lens.mapping Core._Time

-- | Returns only report jobs that were created after the date and time
-- specified in Unix format and Coordinated Universal Time (UTC). For
-- example, the value 1516925490 represents Friday, January 26, 2018
-- 12:11:30 AM.
listReportJobs_byCreationAfter :: Lens.Lens' ListReportJobs (Prelude.Maybe Prelude.UTCTime)
listReportJobs_byCreationAfter = Lens.lens (\ListReportJobs' {byCreationAfter} -> byCreationAfter) (\s@ListReportJobs' {} a -> s {byCreationAfter = a} :: ListReportJobs) Prelude.. Lens.mapping Core._Time

-- | The number of desired results from 1 to 1000. Optional. If unspecified,
-- the query will return 1 MB of data.
listReportJobs_maxResults :: Lens.Lens' ListReportJobs (Prelude.Maybe Prelude.Natural)
listReportJobs_maxResults = Lens.lens (\ListReportJobs' {maxResults} -> maxResults) (\s@ListReportJobs' {} a -> s {maxResults = a} :: ListReportJobs)

instance Core.AWSRequest ListReportJobs where
  type
    AWSResponse ListReportJobs =
      ListReportJobsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListReportJobsResponse'
            Prelude.<$> (x Core..?> "ReportJobs" Core..!@ Prelude.mempty)
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListReportJobs where
  hashWithSalt salt' ListReportJobs' {..} =
    salt' `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` byCreationAfter
      `Prelude.hashWithSalt` byCreationBefore
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` byReportPlanName
      `Prelude.hashWithSalt` byStatus

instance Prelude.NFData ListReportJobs where
  rnf ListReportJobs' {..} =
    Prelude.rnf byStatus
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf byCreationAfter
      `Prelude.seq` Prelude.rnf byCreationBefore
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf byReportPlanName

instance Core.ToHeaders ListReportJobs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListReportJobs where
  toPath = Prelude.const "/audit/report-jobs"

instance Core.ToQuery ListReportJobs where
  toQuery ListReportJobs' {..} =
    Prelude.mconcat
      [ "Status" Core.=: byStatus,
        "ReportPlanName" Core.=: byReportPlanName,
        "NextToken" Core.=: nextToken,
        "CreationBefore" Core.=: byCreationBefore,
        "CreationAfter" Core.=: byCreationAfter,
        "MaxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListReportJobsResponse' smart constructor.
data ListReportJobsResponse = ListReportJobsResponse'
  { -- | Details about your report jobs in JSON format.
    reportJobs :: Prelude.Maybe [ReportJob],
    -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListReportJobsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'reportJobs', 'listReportJobsResponse_reportJobs' - Details about your report jobs in JSON format.
--
-- 'nextToken', 'listReportJobsResponse_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'httpStatus', 'listReportJobsResponse_httpStatus' - The response's http status code.
newListReportJobsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListReportJobsResponse
newListReportJobsResponse pHttpStatus_ =
  ListReportJobsResponse'
    { reportJobs =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Details about your report jobs in JSON format.
listReportJobsResponse_reportJobs :: Lens.Lens' ListReportJobsResponse (Prelude.Maybe [ReportJob])
listReportJobsResponse_reportJobs = Lens.lens (\ListReportJobsResponse' {reportJobs} -> reportJobs) (\s@ListReportJobsResponse' {} a -> s {reportJobs = a} :: ListReportJobsResponse) Prelude.. Lens.mapping Lens.coerced

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listReportJobsResponse_nextToken :: Lens.Lens' ListReportJobsResponse (Prelude.Maybe Prelude.Text)
listReportJobsResponse_nextToken = Lens.lens (\ListReportJobsResponse' {nextToken} -> nextToken) (\s@ListReportJobsResponse' {} a -> s {nextToken = a} :: ListReportJobsResponse)

-- | The response's http status code.
listReportJobsResponse_httpStatus :: Lens.Lens' ListReportJobsResponse Prelude.Int
listReportJobsResponse_httpStatus = Lens.lens (\ListReportJobsResponse' {httpStatus} -> httpStatus) (\s@ListReportJobsResponse' {} a -> s {httpStatus = a} :: ListReportJobsResponse)

instance Prelude.NFData ListReportJobsResponse where
  rnf ListReportJobsResponse' {..} =
    Prelude.rnf reportJobs
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf nextToken
